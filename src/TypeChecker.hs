{-# LANGUAGE OverloadedStrings #-}
module TypeChecker where

import Prelude hiding (lookup)
import Syntax
import Data.HashMap.Strict(HashMap, fromList, insert, foldrWithKey,
    lookup, difference)
import Control.Monad.Trans.Reader(ReaderT(ReaderT), runReaderT, asks, local)
import Control.Monad.Trans.Class(lift)
import Data.List(nub, foldl')
import Control.Monad(when, unless, zipWithM)
import Control.Arrow(first)
import Data.IORef(readIORef, newIORef, modifyIORef', IORef)
import Data.Generics.Uniplate.Data(universe, para, descend)
import Data.Foldable(traverse_)
import Control.Exception(onException)


type Environment = HashMap Name Type

type Substitution = HashMap Id Type

data TypecheckerState =
    TypecheckerState Environment (IORef Integer) (IORef Substitution)

type Typechecker a = ReaderT TypecheckerState IO a

typecheckModule env m =
  do
    r <- newIORef 0
    s <- newIORef mempty
    runReaderT (inferModule m) (TypecheckerState env r s)

inferModule (ModuleDeclaration modName decls) =
  do
    info ("Typechecking Module " ++ renderName modName)
    
    -- Qualify means rename Ty to AnyModuleName.Ty
    let qual = qualifyId modName
    let constructors = foldMap (gatherConstructor qual) decls
    let signatures = fromList' qual (foldMap gatherTypeSig decls)
    
    binds <- with constructors (inferDecls qual generalize signatures decls)
    sanitySkolemCheck binds
    return (mappend constructors (mappend signatures binds))

-- TODO skolems should be caught earlier
sanitySkolemCheck = traverseWithKey_ (\k v ->
    let s = skolems v
    in unless (null s)
        (fail (pretty k ++ " leaked skolems " ++ pretty s)))

traverseWithKey_ f = foldrWithKey (\k v r -> f k v *> r) (pure ())

-- Typecheck Expressions
typecheck :: Expression -> Type -> Typechecker ()
typecheck (LiteralExpression lit) ty =
    typecheckLiteral lit ty
typecheck (Variable x) ty =
    typecheckVar x ty
typecheck (ConstructorExpression c) ty =
    typecheckVar c ty
typecheck (FunctionApplication e1 e2) ty =
  do
    alpha <- newTyVar
    typecheck e1 (TypeArrow alpha ty)
    typecheck e2 alpha
typecheck (CaseExpression expr alts) ty =
  do
    matchTy <- newTyVar
    traverse_ (typecheckAlt matchTy ty) alts
    typecheck expr matchTy
typecheck (LambdaExpression [p] e) (TypeArrow alpha beta) =
    typecheckAlt alpha beta (p, e)
typecheck (LambdaExpression [p] e) ty =
  do
    alpha <- newTyVar
    beta <- newTyVar
    typecheckAlt alpha beta (p, e)
    subsume (TypeArrow alpha beta) ty
typecheck (LetExpression decls e) ty =
  do
    let signatures = fromList' fromId (foldMap gatherTypeSig decls)
    binds <- inferDecls fromId return signatures decls
    with (mappend signatures binds) (typecheck e ty)
typecheck (IfExpression c th el) ty =
  do
    typecheck c (TypeConstructor (fromText "Native.Boolean"))
    typecheck th ty
    typecheck el ty
typecheck (ArrayExpression es) ty =
  do
    alpha <- newTyVar
    traverse_ (flip typecheck alpha) es
    unify (TypeApplication (TypeConstructor (fromText "Native.Array")) alpha) ty
typecheck other _ = fail ("Cannot typecheck expression " ++ show other)

typecheckVar x ty =
  do
    env <- getEnv
    scheme <- mfind x env
    subsume scheme ty

typecheckAlt pty ety (pat, expr)  =
  do
    binds <- typecheckPattern pat pty
    with (fromList' fromId binds) (typecheck expr ety)

-- Typecheck Constructors

{-
capture the signature of all constructors from a type declaration

for example
type Vector a = Vec2 a a | Vec3 a a a
becomes
Vec2 : forall a. a -> a -> Vector a
Vec3 : forall a. a -> a -> a -> Vector a

Vec2 is a constructor and Vector a type constructor
-}
gatherConstructor qual (TypeDeclaration tyIdent vars constructors) =
    let
        qualTy = TypeConstructor (qual tyIdent)
        tyCon = foldl' TypeApplication qualTy (fmap TypeVariable vars)
    in fromList' qual (fmap (fmap (constructorToType tyCon vars)) constructors)
gatherConstructor _ _ = mempty

-- convert a constructor declaration to a type
constructorToType tyCon vars tys =
    scopeCheck (makeForAll vars (foldr TypeArrow tyCon tys))

-- fails type W = Wrapped (a -> a)
-- works type W a = Wrapped (a -> a)
-- works type W = Wrapped (forall a. a -> a)
scopeCheck ty = case freeVars ty of
    [] -> ty
    frees -> error ("Type variables " ++ pretty frees ++ " have no definition")

arrowsToList (TypeArrow x xs) = x:arrowsToList xs
arrowsToList x = [x]

-- Typecheck Bindings
gatherTypeSig (TypeSignature name ty) =
    [(name, makeForAll (freeVars ty) ty)]
gatherTypeSig _ = mempty

-- Assumes that the let bindings are already sorted
-- Let bindings have to be sorted for the translation anyway
inferDecls qual gen signatures =
    foldr (inferDecl qual gen signatures) (return mempty)

-- A version of onException that works with ReaderT
onException' a b = ReaderT (\s -> onException (runReaderT a s) b)

-- If a signature is given, check against it
-- if not, create a new type variable and infer a type
findTypePattern qual signatures (VariablePattern v) =
    maybe newTyVar return (mfind (qual v) signatures)
findTypePattern _ _ _ = newTyVar

inferDecl qual gen signatures (ExpressionDeclaration p e) next = 
  do
    ty <- findTypePattern qual signatures p
    binds <- fmap (fromList' qual) (typecheckPattern p ty)

    -- Overwrite the binds with the more precise signatures
    onException' (with (mappend signatures binds) (typecheck e ty))
        (putStrLn ("When typechecking declaration " ++ pretty p
            ++ " at " ++ locationInfo p))

    -- generalize e.g. id : x1 -> x1 to id : forall x1 . x1 -> x1
    -- Don't generalize signatures
    generalized <- traverse gen (difference binds signatures)

    nextTys <- with generalized next
    return (mappend generalized nextTys)
inferDecl _ _ _ _ next = next

-- Typecheck Patterns
typecheckPattern (VariablePattern x) ty =
    return [(x, ty)]
typecheckPattern (AliasPattern x p) ty =
  do
    binds <- typecheckPattern p ty
    return ((x, ty):binds)
typecheckPattern Wildcard _ =
    return mempty
typecheckPattern (LiteralPattern l) ty =
  do
    typecheckLiteral l ty
    return mempty
typecheckPattern (ConstructorPattern c ps) ty =
  do
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
typecheckPattern (ArrayPattern ps) ty =
  do
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
unify x y =
  do
    subst <- getSubst
    unify' (apply subst x) (apply subst y)

unify' (SkolemConstant x) (SkolemConstant y) | x == y = return ()
unify' (TypeVariable x) (TypeVariable y) | x == y = return ()
unify' (TypeConstructor a) (TypeConstructor b) | a == b = return ()
unify' (TypeVariable x) ty = unifyVar x ty
unify' ty (TypeVariable x) = unifyVar x ty
unify' (TypeApplication f1 e1) (TypeApplication f2 e2) =
  do
    unify f1 f2
    unify e1 e2
unify' (TypeArrow a1 b1) (TypeArrow a2 b2) =
  do
    unify a1 a2
    unify b1 b2
unify' a b =
    fail ("Cannot unify " ++ pretty a ++ " and " ++ pretty b)

unifyVar x ty =
  do
    when (occurs x ty) (fail ("Occurs check: " ++ pretty x
        ++ " occurs in " ++ pretty ty))
    insertSubst x ty

-- Does a type variable occur in a type?
occurs x ty = elem x (freeVars ty)

-- substitute bound variables with unification variables
apply subst (ForAll vs ty) =
    let filteredSubst = excludingKeys vs subst
    in ForAll vs (apply filteredSubst ty)
-- TODO solve infintie loop
-- sometimes the typechecker loops infinitely
-- Presumably here when a variable gets substituted again and again
apply subst ty@(TypeVariable x) =
    maybe ty (apply subst) (lookup x subst)
apply subst ty = descend (apply subst) ty

-- Environment
getEnv = asks (\(TypecheckerState env _ _) -> env)

insertSubst k v =
  do
    r <- asks (\(TypecheckerState _ _ s) -> s)
    modifyRef r (insert k v)

getSubst =
  do
    r <- asks (\(TypecheckerState _ _ s) -> s)
    readRef r

with binds = local (\(TypecheckerState env r s) ->
    TypecheckerState (mappend binds env) r s)

-- Type Variables
newUnique =
  do
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
    let envVars = concatMap freeVars env
    let qualVars = excluding envVars (freeVars ty')
    return (makeForAll qualVars ty')

instantiate (ForAll vars ty) =
  do
    subst <- traverse (\x -> fmap (\t -> (x, t)) newTyVar) vars
    return (apply (fromList subst) ty)
instantiate ty = return ty

subsume x y =
  do
    subst <- getSubst
    subsume' (apply subst x) (apply subst y)

subsume' scheme1 scheme2@(ForAll _ _) =
  do
    (skolVars, ty) <- skolemise scheme2
    subsume scheme1 ty
    let escVars = skolems scheme1 ++ skolems scheme2
    let escaped = including escVars skolVars
    unless (null escaped) (fail ("Escape check: " ++ pretty escaped))
subsume' scheme@(ForAll _ _) t2 =
  do
    t1 <- instantiate scheme
    subsume t1 t2
subsume' (TypeArrow s1 s2) (TypeArrow s3 s4) =
  do
    subsume s3 s1
    subsume s2 s4
subsume' t1 t2 = unify' t1 t2

makeForAll tvs1 (ForAll tvs2 ty) =
    makeForAll (tvs1 ++ tvs2) ty
makeForAll tvs ty =
    case nub tvs of
        [] -> ty
        is -> ForAll is ty

skolemise (ForAll vars ty) =
  do
    skolVars <- traverse (const newUniqueName) vars
    let subs = fromList (zip vars (fmap SkolemConstant skolVars))
    return (skolVars, apply subs ty)
skolemise ty = return ([], ty)

info s = lift (putStrLn s)

readRef s = lift (readIORef s)

modifyRef s f = lift (modifyIORef' s f)

fromList' qual l = fromList (fmap (first qual) l)
