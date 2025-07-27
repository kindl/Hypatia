{-# LANGUAGE OverloadedStrings #-}
module Typechecker where

import Prelude hiding (lookup)
import Syntax
import Data.Word(Word64)
import Data.HashMap.Strict(HashMap, fromList, insert, foldrWithKey, lookup, singleton)
import qualified Data.HashSet as Set
import Data.Monoid(getAp)
import Control.Monad.Trans.Reader(ReaderT(ReaderT), runReaderT, asks, local)
import Control.Monad.Trans.Class(lift)
import Data.List(sortOn)
import Control.Monad(when, unless, zipWithM)
import Data.IORef(readIORef, newIORef, modifyIORef', IORef)
import Data.Generics.Uniplate.Data(universe, para, descend, transformM)
import Data.Foldable(traverse_, foldMap', foldl')
import Control.Exception(onException)
import Control.Applicative((<|>))

type Environment = HashMap Name Type

type Substitution = HashMap Id Type

data TypecheckerState =
    TypecheckerState Environment (IORef Word64) (IORef Substitution)

type Typechecker a = ReaderT TypecheckerState IO a

typecheckModule env m = do
    r <- newIORef 0
    s <- newIORef mempty
    putStrLn ("Typechecking Module " ++ renderName (getName m))
    runReaderT (inferModule m) (TypecheckerState env r s)

inferModule (ModuleDeclaration _ _ decls) = do
    constructors <- getAp (foldMap' gatherConstructor decls)
    signatures <- getAp (foldMap' gatherTypeSig decls)

    let consAndSigs = constructors <> signatures
    binds <- with consAndSigs (inferDecls generalize decls)

    sanitySkolemCheck binds
    sanityFreeVariableCheck binds

    return (consAndSigs <> binds)

-- Skolem variables are caught earlier,
-- so this check is redundant, but makes sure
-- that no skolem variables make it to the top level
sanitySkolemCheck = traverseWithKey_ (\k v ->
    let s = skolems v
    in unless (null s)
        (fail ("Bug: " ++ renderError k ++ " leaked skolems " ++ renderSetToError s)))

sanityFreeVariableCheck = traverseWithKey_ (\k v ->
    let vs = freeVars v
    in unless (null vs)
        (fail ("Bug: " ++ renderError k ++ " leaked free variable " ++ renderSetToError vs)))

traverseWithKey_ f = foldrWithKey (\k v r -> f k v *> r) (pure ())

-- Typecheck Expressions
typecheck :: Expression -> Type -> Typechecker ()
typecheck (LiteralExpression lit) ty =
    typecheckLiteral lit ty
typecheck (Variable x) ty =
    typecheckVar x ty
typecheck (ConstructorExpression c) ty =
    typecheckVar c ty
typecheck (FunctionApplication e1 e2) ty = do
    alpha <- newTyVar
    typecheck e1 (TypeArrow alpha ty)
    typecheck e2 alpha
typecheck (CaseExpression expr alts) ty = do
    patternTy <- newTyVar
    traverse_ (uncurry (typecheckAlt patternTy ty)) alts
    typecheck expr patternTy
typecheck (LambdaExpression ps e) ty =
    typecheckLambda ps e ty
typecheck (LetExpression decls e) ty = do
    signatures <- getAp (foldMap' gatherTypeSig decls)
    binds <- with signatures (inferDecls return decls)
    with (signatures <> binds) (typecheck e ty)
typecheck (IfExpression condtition thenBranch elseBranch) ty = do
    typecheck condtition (TypeConstructor (fromText "Native.Bool"))
    typecheck thenBranch ty
    typecheck elseBranch ty
typecheck (ArrayExpression es) ty = do
    elementTy <- newTyVar
    traverse_ (flip typecheck elementTy) es
    unify (TypeApplication (TypeConstructor (fromText "Native.Array")) elementTy) ty
typecheck other _ = fail ("Cannot typecheck expression " ++ show other)

typecheckVar x ty = do
    env <- getEnv
    scheme <- mfind x env
    instantiated <- deepInstantiate scheme
    subsume instantiated ty

-- A shortcut that avoids generating new type variables
typecheckLambda [p] e (TypeArrow patternTy resultTy) =
    typecheckAlt patternTy resultTy p e
typecheckLambda [p] e s@(ForAll _ (TypeArrow _ _)) = do
    (skolVars, TypeArrow alpha beta) <- skolemise s
    typecheckAlt alpha beta p e
    subst <- getSubst
    env <- getEnv
    let escVars = skolems s <> foldMap' (skolems . apply subst) env
    let escaped = Set.intersection escVars (Set.fromList skolVars)
    unless (null escaped) (fail ("Escape check lambda: "
        ++ renderSetToError escaped ++ " escaped when checking fun "
        ++ renderError p ++ " -> ... against " ++ renderError s))
typecheckLambda [p] e ty = do
    patternTy <- newTyVar
    resultTy <- newTyVar
    typecheckAlt patternTy resultTy p e
    subsume (TypeArrow patternTy resultTy) ty
typecheckLambda ps _ _ =
    fail ("Lambda with patterns " ++ show ps ++ " was not curried")

typecheckAlt patternTy expressionTy pat expr = do
    binds <- typecheckPattern pat patternTy
    typecheckBraced (fromList binds) (renderError pat) expr expressionTy

{-
Typecheck Constructors

capture the signature of all constructors from a type declaration

for example
type Vector a = Vec2 a a | Vec3 a a a
becomes
Vec2 : forall a. a -> a -> Vector a
Vec3 : forall a. a -> a -> a -> Vector a

Vec2 is a constructor and Vector a type constructor
-}
gatherConstructor (TypeDeclaration tyIdent vars constructors) = do
    vars' <- toSetUniqueM vars
    let resultTy = TypeConstructor tyIdent
    let tyCon = foldl' TypeApplication resultTy (fmap TypeVariable vars)
    fmap fromList (traverse (traverse (constructorToType tyCon vars')) constructors)
gatherConstructor _ = pure mempty

-- Convert a constructor declaration to a type
-- Example for variable scope checks:
-- fails type W = Wrapped (a -> a)
-- works type W a = Wrapped (a -> a)
-- works type W = Wrapped (forall a. a -> a)
constructorToType tyCon vars tys =
    let
        constructorTy = makeForAll vars (foldr TypeArrow tyCon tys)
        frees = freeVars constructorTy
    in if null frees
        then pure constructorTy
        else fail ("Constructor type variables " ++ renderSetToError frees ++ " have no definition")

-- move types from signatures into environment
gatherTypeSig (TypeSignature name ty@(ForAll _ _)) =
    let frees = freeVars ty
    in if null frees
        then pure (singleton name ty)
        else fail ("Type signature " ++ renderError name ++
            " has following variables " ++ renderSetToError frees ++ " without definition")
gatherTypeSig (TypeSignature name ty) =
    pure (singleton name (makeForAll (freeVars ty) ty))
gatherTypeSig _ = pure mempty

-- Assumes that the let bindings are already sorted
-- Let bindings have to be sorted for the translation anyway
inferDecls gen =
    foldr (inferDecl gen) (return mempty)

-- A version of onException that works with ReaderT
onException' a b =
    ReaderT (\s -> onException (runReaderT a s) b)

inferDecl gen (FunctionDeclaration v alts) next = do
    env <- getEnv
    ty <- mfind v env <|> newTyVarAt (getLocation (getId v))
    let binds' = fromList [(v, ty)]
    -- TODO use typecheckLambda as soon as it supports multiple patterns
    let exprs = fmap (uncurry curryLambda) alts
    traverse_ (\e -> typecheckBraced binds' (renderError v) e ty) exprs
    typecheckNextWith gen binds' next
inferDecl gen (ExpressionDeclaration (VariablePattern v) e) next = do
    env <- getEnv
    ty <- mfind v env <|> newTyVarAt (getLocation (getId v))
    let binds' = fromList [(v, ty)]
    typecheckBraced binds' (renderError v) e ty
    typecheckNextWith gen binds' next
inferDecl gen (ExpressionDeclaration p e) next = do
    ty <- newTyVar
    binds <- typecheckPattern p ty
    let binds' = fromList binds
    typecheckBraced binds' (renderError p) e ty
    typecheckNextWith gen binds' next
inferDecl _ _ next = next

-- Wrap typecheck with onError to give more info
typecheckBraced binds errInfo e ty = do
    onException' (with binds (typecheck e ty)) (
        let baseError = "When typechecking declaration " ++ errInfo
        in case e of
            -- If an error occurs in a function declaration,
            -- also show in which pattern it happened
            LambdaExpression p _ -> putStrLn (baseError ++ "\n   specifically at " ++ renderError p)
            _ -> putStrLn baseError)

typecheckNextWith gen binds next = do
    -- generalize e.g. id : x1 -> x1 to id : forall x1 . x1 -> x1
    -- NOTE if binds contain signatures then those are not generalized
    -- because they are already in the form forall x1 ... xn . t
    -- also in LetExpressions gen is just 'return' and does nothing
    generalized <- traverse gen binds

    nextTys <- with generalized next
    return (generalized <> nextTys)

-- Typecheck Patterns
typecheckPattern (VariablePattern x) ty = do
    return [(x, ty)]
typecheckPattern (AliasPattern x p) ty = do
    binds <- typecheckPattern p ty
    return ((x, ty):binds)
typecheckPattern (Wildcard _) _ =
    return mempty
typecheckPattern (LiteralPattern l) ty = do
    typecheckLiteral l ty
    return mempty
typecheckPattern (ConstructorPattern _ c ps) ty = do
    env <- getEnv
    scheme <- mfind c env
    consTy <- instantiate scheme

    let tys = arrowsToList consTy
    let resultTy = last tys
    let consTys = init tys

    when (length ps /= length consTys)
        (fail ("Constructor " ++ renderError c
            ++ " was given wrong number of arguments"))
    binds <- zipWithM typecheckPattern ps consTys
    unify resultTy ty
    return (mconcat binds)
typecheckPattern (ArrayPattern ps) ty = do
    elementTy <- newTyVar
    binds <- traverse (\p -> typecheckPattern p elementTy) ps
    unify (TypeApplication (TypeConstructor (fromText "Native.Array")) elementTy) ty
    return (mconcat binds)
typecheckPattern other _ =
    fail ("Cannot typecheck pattern " ++ renderError other)

-- Typecheck Literals
typecheckLiteral (Number _) ty =
    unify (TypeConstructor (fromText "Native.Number")) ty
typecheckLiteral (Text _) ty =
    unify (TypeConstructor (fromText "Native.Text")) ty

-- Unification
unify x y = do
    subst <- getSubst
    unify' (apply subst x) (apply subst y)

unify' (SkolemConstant x) (SkolemConstant y) | x == y = return ()
unify' (TypeVariable x) (TypeVariable y) | x == y = return ()
unify' (TypeConstructor a) (TypeConstructor b) | a == b = return ()
-- When unifying, no higher rank type should appear
unify' s@(ForAll _ _) ty =
    fail ("Cannot unify scheme:\n" ++ renderError s ++ "\nand\n" ++ renderError ty)
unify' ty s@(ForAll _ _) =
    fail ("Cannot unify:\n" ++ renderError ty ++ "\nand scheme\n" ++ renderError s)
unify' (TypeVariable x) ty = unifyVar x ty
unify' ty (TypeVariable x) = unifyVar x ty
unify' (TypeApplication f1 e1) (TypeApplication f2 e2) = do
-- After unifying f1 and f2 the substitution might have changed.
-- Therefore, unify is used instead of unify'
    unify' f1 f2 *> unify e1 e2
unify' (TypeArrow a1 b1) (TypeArrow a2 b2) =
    unify' a1 a2 *> unify b1 b2
unify' a b =
    fail ("Cannot unify:\n" ++ renderError a ++ "\nand\n" ++ renderError b)

unifyVar x ty = if occurs x ty
    then fail ("Occurs check: " ++ renderError x ++ " occurs in " ++ renderError ty)
    else insertSubst x ty

-- Does a type variable occur in a type?
occurs x ty = elem x (freeVars ty)

-- substitute bound variables with unification variables
apply subst (ForAll vs ty) =
    let filteredSubst = differenceKeys subst vs
    in ForAll vs (apply filteredSubst ty)
-- TODO investigate infinite loop
-- sometimes the typechecker looped infinitely
-- substituting a variable again and again
-- it is not clear why this happens
-- as this should be caught in the occurs check
apply subst ty@(TypeVariable x) =
    maybe ty (\ty2 -> case ty2 of
            -- do not substitute if the variable
            -- would be substituted with itself
            TypeVariable y | x == y -> ty2
            _ -> apply subst ty2) (lookup x subst)
apply subst ty = descend (apply subst) ty

-- Environment
getEnv = asks (\(TypecheckerState env _ _) -> env)

insertSubst k v = do
    r <- asks (\(TypecheckerState _ _ s) -> s)
    lift (modifyIORef' r (insert k v))

getSubst = do
    r <- asks (\(TypecheckerState _ _ s) -> s)
    lift (readIORef r)

with binds = local (\(TypecheckerState env r s) ->
    TypecheckerState (binds <> env) r s)

-- Type Variables
newUnique = do
    r <- asks (\(TypecheckerState _ u _) -> u)
    lift (modifyIORef' r succ >> readIORef r)

newTyVarAt location = fmap TypeVariable (newUniqueName location)

newTyVar = newTyVarAt builtinLocation

newUniqueName location = fmap (prefixedId location . uintToText) newUnique

-- extract skolem constants
skolems ty = Set.fromList [c | SkolemConstant c <- universe ty]

-- extract free variables
freeVars ty =
    let
        f (ForAll vars _) cs = Set.difference (mconcat cs) (Set.fromList vars)
        f (TypeVariable v) _ = Set.singleton v
        f _ cs = mconcat cs
    in para f ty

generalize ty = do
    subst <- getSubst
    let ty' = apply subst ty
    env <- getEnv
    let envVars = foldMap' (freeVars . apply subst) env
    let qualVars = Set.difference (freeVars ty') envVars
    return (makeForAll qualVars ty')

instantiate (ForAll vars ty) = do
    subst <- traverse (\x -> fmap (\t -> (x, t)) (newTyVarAt (getLocation x))) vars
    return (apply (fromList subst) ty)
instantiate ty = return ty

-- TODO: find better solution
-- Deep instantiation is a bandaid for typing functions like
-- `map3 map2 f a1 a2 a3 = map2 identity (map2 f a1 a2) a3`
-- However, it leads to programs being accepted that should not be,
-- for example `runST (newSTRef 0)`
deepInstantiate ty = transformM instantiate ty

subsume x y = do
    subst <- getSubst
    subsume' (apply subst x) (apply subst y)

{-
This typechecker roughly follows the tutorial of
"Practical type inference for arbitrary-rank types"
However, subsumption was kept simple with the intention to
complete it when needed.
Meanwhile, the ghc proposal "Simplify subsumption" proposed
to keep subsumption simple anyway
-}
subsume' scheme1 scheme2@(ForAll _ _) = do
    (skolVars, ty) <- skolemise scheme2
    subsume' scheme1 ty
    subst <- getSubst
    let escVars = skolems (apply subst scheme1) <> skolems (apply subst scheme2)
    let escaped = Set.intersection escVars (Set.fromList skolVars)
    unless (null escaped) (fail ("Escape check: " ++ renderSetToError escaped
        ++ "\nescaped when subsuming\n" ++ renderError scheme1
        ++ "\nand\n" ++ renderError scheme2))
subsume' scheme@(ForAll _ _) t2 = do
    t1 <- instantiate scheme
    subsume' t1 t2
subsume' t1 t2 = unify' t1 t2

-- Normalizes foralls
-- forall a. forall b. t becomes forall a b. t
-- forall a a. t becomes forall a. t
makeForAll tvs1 (ForAll tvs2 ty) =
    makeForAll (tvs1 <> Set.fromList tvs2) ty
makeForAll tvs ty =
    if null tvs then ty else ForAll (sortOn getText (Set.toList tvs)) ty

skolemise (ForAll vars ty) = do
    skolVars <- traverse (newUniqueName . getLocation) vars
    let subs = fromList (zip vars (fmap SkolemConstant skolVars))
    return (skolVars, apply subs ty)
skolemise ty = return ([], ty)
