{-# LANGUAGE OverloadedStrings #-}
module Typechecker where

import Prelude hiding (lookup)
import Syntax
import Data.Word(Word64)
import Data.HashMap.Strict(HashMap, fromList, insert, foldrWithKey, lookup)
import Data.Monoid(getAp)
import Control.Monad.Trans.Reader(ReaderT(ReaderT), runReaderT, asks, local)
import Control.Monad.Trans.Class(lift)
import Data.List(nub, foldl')
import Data.Maybe(isNothing)
import Control.Monad(when, unless, zipWithM)
import Data.IORef(readIORef, newIORef, modifyIORef', IORef)
import Data.Generics.Uniplate.Data(universe, para, descend)
import Data.Foldable(traverse_, foldMap')
import Control.Exception(onException)


type Environment = HashMap Name Type

type Substitution = HashMap Id Type

data TypecheckerState =
    TypecheckerState Environment (IORef Word64) (IORef Substitution)

type Typechecker a = ReaderT TypecheckerState IO a

typecheckModule env m = do
    r <- newIORef 0
    s <- newIORef mempty
    runReaderT (inferModule m) (TypecheckerState env r s)

inferModule (ModuleDeclaration modName decls) = do
    info ("Typechecking Module " ++ renderName modName)

    constructors <- getAp (foldMap' gatherConstructor decls)
    let signatures = fromList (foldMap' gatherTypeSig decls)

    let types = mappend constructors signatures
    -- The signatures are also passed as a plain argument for lookups
    binds <- with types (inferDecls generalize signatures decls)
    
    sanitySkolemCheck binds
    sanityFreeVariableCheck binds
    
    return (mappend types binds)

-- Skolem variables are caught earlier,
-- so this check is redundant, but makes sure
-- that no skolem variables make it to the top level
sanitySkolemCheck = traverseWithKey_ (\k v ->
    let s = skolems v
    in unless (null s)
        (fail ("Bug: " ++ pretty k ++ " leaked skolems " ++ pretty s)))

sanityFreeVariableCheck = traverseWithKey_ (\k v ->
    let vs = freeVars v
    in unless (null vs)
        (fail ("Bug: " ++ pretty k ++ " leaked free variable " ++ pretty vs)))

traverseWithKey_ f = foldrWithKey (\k v r -> f k v *> r) (pure ())

-- Typecheck Expressions
typecheck :: Expression -> Type -> Typechecker ()
typecheck (LiteralExpression lit) ty =
    typecheckLiteral lit ty
typecheck (Variable x) ty =
    typecheckVar x ty
typecheck (ConstructorExpression c) ty =
    typecheckVar c ty
-- A possible shortcut that avoids generating new type variables
-- because the type of x does not have to be inferred but can be read of
-- similar to typecheckPattern (ConstructorPattern c ps)
-- typecheck (FunctionApplication (Variable x) e) ty =
typecheck (FunctionApplication e1 e2) ty = do
    alpha <- newTyVar
    typecheck e1 (TypeArrow alpha ty)
    typecheck e2 alpha
typecheck (CaseExpression expr alts) ty = do
    pty <- newTyVar
    traverse_ (uncurry (typecheckAlt pty ty)) alts
    typecheck expr pty
-- A common shortcut that avoids generating new type variables
typecheck (LambdaExpression [p] e) (TypeArrow pty ety) =
    typecheckAlt pty ety p e
-- A shortcut that avoids generating new type variables
typecheck (LambdaExpression [p] e) s@(ForAll _ (TypeArrow _ _)) = do
    (skolVars, TypeArrow alpha beta) <- skolemise s
    typecheckAlt alpha beta p e
    subst <- getSubst
    env <- getEnv
    let escVars = skolems s ++ foldMap' (skolems . apply subst) env
    let escaped = including escVars skolVars
    unless (null escaped) (fail ("Escape check lambda: "
        ++ pretty escaped ++ " escaped when checking fun "
        ++ pretty p ++ " -> ... against " ++ pretty s))
typecheck (LambdaExpression [p] e) ty = do
    alpha <- newTyVar
    beta <- newTyVar
    typecheckAlt alpha beta p e
    subsume (TypeArrow alpha beta) ty
typecheck (LetExpression decls e) ty = do
    let signatures = fromList (foldMap' gatherTypeSig decls)
    binds <- with signatures (inferDecls return signatures decls)
    with (mappend signatures binds) (typecheck e ty)
typecheck (IfExpression c th el) ty = do
    typecheck c (TypeConstructor (fromText "Native.Boolean"))
    typecheck th ty
    typecheck el ty
typecheck (ArrayExpression es) ty = do
    alpha <- newTyVar
    traverse_ (flip typecheck alpha) es
    unify (TypeApplication (TypeConstructor (fromText "Native.Array")) alpha) ty
typecheck other _ = fail ("Cannot typecheck expression " ++ show other)

typecheckVar x ty = do
    env <- getEnv
    scheme <- mfind x env
    subsume scheme ty

typecheckAlt pty ety pat expr = do
    binds <- typecheckPattern pat pty
    with (fromList binds) (typecheck expr ety)

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
gatherConstructor (TypeDeclaration tyIdent vars constructors) =
    let
        resTy = TypeConstructor tyIdent
        tyCon = foldl' TypeApplication resTy (fmap TypeVariable vars)
    in fmap fromList (traverse (traverse (constructorToType tyCon vars)) constructors)
gatherConstructor _ = pure mempty

-- convert a constructor declaration to a type
constructorToType tyCon vars tys =
    scopeCheck (makeForAll vars (foldr TypeArrow tyCon tys))

-- fails type W = Wrapped (a -> a)
-- works type W a = Wrapped (a -> a)
-- works type W = Wrapped (forall a. a -> a)
scopeCheck ty = case freeVars ty of
    [] -> pure ty
    frees -> fail ("Type variables " ++ pretty frees ++ " have no definition")

arrowsToList (TypeArrow x xs) = x:arrowsToList xs
arrowsToList x = [x]

-- Typecheck Bindings
gatherTypeSig (TypeSignature name ty) =
    [(name, makeForAll (freeVars ty) ty)]
gatherTypeSig _ = mempty

-- Assumes that the let bindings are already sorted
-- Let bindings have to be sorted for the translation anyway
inferDecls gen signatures =
    foldr (inferDecl gen signatures) (return mempty)

-- A version of onException that works with ReaderT
onException' a b = ReaderT (\s -> onException (runReaderT a s) b)

-- If a signature is given, check against it
-- otherwise create a new type variable and infer a type
-- TODO match variable against its signature in cases like this:
-- x : Int
-- Tuple x _ = Tuple "hi" "ho"
-- Should be a type error
findSignature signatures (VariablePattern v) =
    maybe newTyVar return (lookup v signatures)
findSignature _ _ = newTyVar

bindsWithoutSignatures signatures binds =
    filter (\x -> isNothing (lookup (fst x) signatures)) binds

inferDecl gen signatures (ExpressionDeclaration p e) next = do
    ty <- findSignature signatures p
    binds <- typecheckPattern p ty
    let binds' = fromList (bindsWithoutSignatures signatures binds)

    -- If an error occurs, show in which declaration it happened
    onException' (with binds' (typecheck e ty))
        (putStrLn ("When typechecking declaration " ++ pretty p
            ++ " at " ++ locationInfo p))

    -- generalize e.g. id : x1 -> x1 to id : forall x1 . x1 -> x1
    -- NOTE if binds contain signatures then those are not generalized
    -- because they are already in the form forall x1 ... xn . t
    -- also in LetExpressions gen is just 'return' and does nothing
    generalized <- traverse gen binds'

    nextTys <- with generalized next
    return (mappend generalized nextTys)
inferDecl _ _ _ next = next

-- Typecheck Patterns
typecheckPattern (VariablePattern x) ty =
    return [(x, ty)]
typecheckPattern (AliasPattern x p) ty = do
    binds <- typecheckPattern p ty
    return ((x, ty):binds)
typecheckPattern (Wildcard _) _ =
    return mempty
typecheckPattern (LiteralPattern l) ty = do
    typecheckLiteral l ty
    return mempty
typecheckPattern (ConstructorPattern c ps) ty = do
    env <- getEnv
    scheme <- mfind c env
    consTy <- instantiate scheme

    let tys = arrowsToList consTy
    let resultTy = last tys
    let consTys = init tys

    when (length ps /= length consTys)
        (fail ("Constructor " ++ pretty c
            ++ " was given wrong number of arguments"))
    binds <- zipWithM typecheckPattern ps consTys
    unify resultTy ty
    return (mconcat binds)
typecheckPattern (ArrayPattern ps) ty = do
    alpha <- newTyVar
    binds <- traverse (\p -> typecheckPattern p alpha) ps
    unify (TypeApplication (TypeConstructor (fromText "Native.Array")) alpha) ty
    return (mconcat binds)
typecheckPattern other _ =
    fail ("Cannot typecheck pattern " ++ pretty other)

-- Typecheck Literals
typecheckLiteral (Numeral _) ty =
    unify (TypeConstructor (fromText "Native.Numeral")) ty
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
    fail ("Cannot unify scheme " ++ pretty s ++ " and " ++ pretty ty)
unify' ty s@(ForAll _ _) =
    fail ("Cannot unify " ++ pretty ty ++ " and scheme " ++ pretty s)
unify' (TypeVariable x) ty = unifyVar x ty
unify' ty (TypeVariable x) = unifyVar x ty
-- After unifying f1 and f2 the substitution might have changed
-- and therefore needs to be reapplied to e1 and e2
unify' (TypeApplication f1 e1) (TypeApplication f2 e2) =
    unify' f1 f2 *> unify e1 e2
unify' (TypeArrow a1 b1) (TypeArrow a2 b2) =
    unify' a1 a2 *> unify b1 b2
unify' a b =
    fail ("Cannot unify " ++ pretty a ++ " and " ++ pretty b)

unifyVar x ty = if occurs x ty
    then fail ("Occurs check: " ++ pretty x ++ " occurs in " ++ pretty ty)
    else insertSubst x ty

-- Does a type variable occur in a type?
occurs x ty = elem x (freeVars ty)

-- substitute bound variables with unification variables
apply subst (ForAll vs ty) =
    let filteredSubst = excludingKeys vs subst
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
    modifyRef r (insert k v)

getSubst = do
    r <- asks (\(TypecheckerState _ _ s) -> s)
    readRef r

with binds = local (\(TypecheckerState env r s) ->
    TypecheckerState (mappend binds env) r s)

-- Type Variables
newUnique = do
    r <- asks (\(TypecheckerState _ u _) -> u)
    modifyRef r succ
    readRef r

newTyVar = fmap TypeVariable newUniqueName

newUniqueName = fmap (prefixedId . show) newUnique

-- extract skolem constants
skolems ty = [c | SkolemConstant c <- universe ty]

-- extract free variables
freeVars ty =
    let
        f (ForAll vars _) cs = excluding vars (concat cs)
        f (TypeVariable v) _ = [v]
        f _ cs = concat cs
    in para f ty

generalize ty = do
    subst <- getSubst
    let ty' = apply subst ty
    env <- getEnv
    let envVars = foldMap' (freeVars . apply subst) env
    let qualVars = excluding envVars (freeVars ty')
    return (makeForAll qualVars ty')

instantiate (ForAll vars ty) = do
    subst <- traverse (\x -> fmap (\t -> (x, t)) newTyVar) vars
    return (apply (fromList subst) ty)
instantiate ty = return ty

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
    let escVars = skolems (apply subst scheme1) ++ skolems (apply subst scheme2)
    let escaped = including escVars skolVars
    unless (null escaped) (fail ("Escape check: " ++ pretty escaped
        ++ " escaped when subsuming " ++ pretty scheme1
        ++ " and " ++ pretty scheme2))
subsume' scheme@(ForAll _ _) t2 = do
    t1 <- instantiate scheme
    subsume' t1 t2
subsume' t1 t2 = unify' t1 t2

-- Normalizes foralls
-- forall a. forall b. t becomes forall a b. t 
-- forall a a. t becomes forall a. t 
makeForAll tvs1 (ForAll tvs2 ty) =
    makeForAll (tvs1 ++ tvs2) ty
makeForAll [] ty = ty
makeForAll tvs ty = ForAll (nub tvs) ty

skolemise (ForAll vars ty) = do
    skolVars <- traverse (const newUniqueName) vars
    let subs = fromList (zip vars (fmap SkolemConstant skolVars))
    return (skolVars, apply subs ty)
skolemise ty = return ([], ty)

info s = lift (putStrLn s)

readRef s = lift (readIORef s)

modifyRef s f = lift (modifyIORef' s f)
