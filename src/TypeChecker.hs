module TypeChecker where

import Syntax
import Data.HashMap.Strict(HashMap, fromList, filterWithKey)
import Control.Monad.Trans.Reader(ReaderT, runReaderT, ask, local)
import Control.Monad.Trans.Class(lift)
import Control.Arrow(first)
import Data.List(nub)
import Data.Maybe(fromMaybe)
import Control.Monad(when, zipWithM)
import Data.IORef(readIORef, writeIORef, newIORef, IORef)
import Data.Generics.Uniplate.Data(universe, para, transformM, descend)


type Environment = HashMap Name Type

data TypecheckerState = TypecheckerState Environment (IORef Integer)

type Typechecker a = ReaderT TypecheckerState IO a

typecheckModule env m =
  do
    r <- newIORef 0
    runReaderT (inferModule m) (TypecheckerState env r)

inferModule (ModuleDeclaration modName decls) =
  do
    info "---------------------------------"
    info ("Typechecking Module " ++ pretty modName)
    let enums = foldMap (gatherEnum (qualifyId modName)) decls
    let signatures = foldMap gatherTypeSig decls
    let types = mapKeys (qualifyId modName) (enums `mappend` signatures)
    binds <- with types (inferDecls (qualifyId modName) generalize decls)
    return (types `mappend` binds)

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
typecheck (CaseLambdaExpression alts) ty =
  do
    alpha <- newTyVar
    beta <- newTyVar
    mapM_ (typecheckAlt alpha beta) alts
    subsume (TypeArrow alpha beta) ty
typecheck (LambdaExpression [p] e) ty =
  do
    alpha <- newTyVar
    beta <- newTyVar
    info ("Typechecking lambda " ++ pretty p)
    typecheckAlt alpha beta (p, e)
    subsume (TypeArrow alpha beta) ty
typecheck (LetExpression decls e) ty =
  do
    let types = mapKeys fromId (foldMap gatherTypeSig decls)
    binds <- with types (inferDecls fromId return decls)
    with binds (typecheck e ty)
typecheck (IfExpression c th el) ty =
  do
    typecheck c (TypeConstructor (fromString "Prelude.Boolean"))
    typecheck th ty
    typecheck el ty
typecheck (ArrayExpression es) ty =
  do
    alpha <- newTyVar
    mapM_ (flip typecheck alpha) es
    unify (TypeApplication (TypeConstructor (fromString "Native.Array")) alpha) ty
typecheck other _ = fail ("Typecheck " ++ show other)

typecheckVar x ty =
  do
    env <- getEnv
    scheme <- mfind x env
    subsume scheme ty

typecheckAlt pty ety (pat, expr)  =
  do
    binds <- typecheckPattern pat pty
    let env = mapKeys fromId binds
    with env (typecheck expr ety)

-- Typecheck Constructors

-- capture enums from declarations for the type environment
gatherEnum qual (EnumDeclaration id vars constructors) =
    let enumName = TypeConstructor (qual id)
    in foldMap (constructorToType enumName vars) constructors
gatherEnum _ _ = mempty

-- convert a constructor declaration to a type
constructorToType ty vars (name, tys) =
    let
        res = foldl TypeApplication ty (fmap TypeVariable vars)
        resTy = foldr TypeArrow res tys
        frees = excluding vars (bounds resTy)
    in case frees of
        [] -> singleton name (makeForAll vars resTy)
        _  -> error ("Type variables " ++ pretty frees
               ++ " appear free in " ++ pretty name )

arrowsToList (TypeArrow x xs) = x:arrowsToList xs
arrowsToList x = [x]

-- Typecheck Bindings
gatherTypeSig (TypeSignature name ty) =
    singleton name (makeForAll (bounds ty) ty)
gatherTypeSig _ = mempty

-- the function assumes that the let bindings are already sorted
-- we have to sort the let bindings for the translation anyway
-- but we do not want to transform the bindings before type checking
-- TODO solve without sorting
-- TODO type annotations
inferDecls qual gen (ExpressionDeclaration p e:decls) =
  do
    info ("Typechecking declaration " ++ pretty p)

    ty <- newTyVar
    binds <- fmap (mapKeys qual) (typecheckPattern p ty)
    with binds (typecheck e ty)

    generalized <- traverse gen binds
    next <- with generalized (inferDecls qual gen decls)
    return (next `mappend` generalized)
inferDecls qual gen (_:decls) = inferDecls qual gen decls
inferDecls _ _ [] = return mempty

-- Typecheck Patterns
typecheckPattern (VariablePattern x) ty =
    return (singleton x ty)
typecheckPattern (AliasPattern x p) ty =
  do
    binds <- typecheckPattern p ty
    return (singleton x ty `mappend` binds)
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
        (fail ("Constructor " ++ pretty c ++ " was given wrong number of arguments"))
    binds <- zipWithM typecheckPattern ps consTys
    unify resultTy ty
    return (mconcat binds)
typecheckPattern (ArrayPattern ps) ty =
  do
    alpha <- newTyVar
    binds <- traverse (flip typecheckPattern alpha) ps
    unify (TypeApplication (TypeConstructor (fromString "Native.Array")) alpha) ty
    return (mconcat binds)
typecheckPattern other _ = fail ("Cannot typecheck pattern " ++ pretty other)

-- Typecheck Literals
typecheckLiteral (Numeral _) ty =
    unify (TypeConstructor (fromString "Native.Numeral")) ty
typecheckLiteral (Text _) ty =
    unify (TypeConstructor (fromString "Native.Text")) ty

-- Unification
unify (SkolemConstant x) (SkolemConstant y) | x == y = return ()
unify (UniVariable r1) (UniVariable r2) | r1 == r2 = return ()
unify (UniVariable r1) ty = unifyVar r1 ty
unify ty (UniVariable r1) = unifyVar r1 ty
unify (TypeApplication f1 e1) (TypeApplication f2 e2) =
  do
    unify f1 f2
    unify e1 e2
unify (TypeArrow a1 b1) (TypeArrow a2 b2) =
  do
    unify a1 a2
    unify b1 b2
unify (TypeConstructor a) (TypeConstructor b) =
    when (a /= b) (fail ("Cannot unify type constructors " ++ pretty a ++ " and " ++ pretty b))
unify a b =
    fail ("Cannot unify " ++ pretty a ++ " and " ++ pretty b)

unifyVar r ty2 = do
    mty <- readRef r
    case mty of
        Just ty1 -> unify ty1 ty2
        Nothing -> case ty2 of
            UniVariable r2 -> do
                mty2 <- readRef r2
                case mty2 of
                    Just ty -> unify (UniVariable r) ty
                    Nothing -> writeRef r (Just (UniVariable r2))
            _ -> do
                when (occurs r ty2) (fail ("occurs in " ++ pretty ty2))
                writeRef r (Just ty2)

-- Does a type variable occur in a type?
occurs x ty = elem x (unis ty)

-- substitute bound variables with unification variables
apply subst (ForAll vs ty) =
    let filteredSubst = filterWithKey (\k _ -> notElem k vs) subst
    in ForAll vs (apply filteredSubst ty)
apply subst ty@(TypeVariable x) = fromMaybe ty (mfind x subst)
apply subst ty = descend (apply subst) ty

-- substitute unification variables with types
zonk ty =
    let
        f t@(UniVariable r) =
            do
                mty <- readRef r
                maybe (return t) zonk mty
        f t = return t
    in transformM f ty

-- Environment
getEnv = fmap (\(TypecheckerState env _) -> env) ask

with binds =
    local (\(TypecheckerState env r) -> TypecheckerState (mappend binds env) r)

-- Type Variables
newUnique =
  do
    TypecheckerState _ r <- ask
    i <- readRef r
    writeRef r (i + 1)
    return i

newTyVar = fmap UniVariable (newRef Nothing)

-- extract unification variables
unis ty = [u | UniVariable u <- universe ty]

-- extract skolem constants
skolems ty = [c | SkolemConstant c <- universe ty]

-- extract bound variables
bounds ty =
    let
        f (ForAll vars _) cs = excluding vars (concat cs)
        f (TypeVariable v) _ = [v]
        f _ cs = concat cs
    in para f ty

freeVars ty = fmap unis (zonk ty)

newBound r = do
    n <- newUniqueName
    writeRef r (Just (TypeVariable n))
    return n

generalize ty = do
    env <- getEnv
    envVars <- traverse freeVars env
    qualVars <- fmap (excluding (concat envVars)) (freeVars ty)
    ids <- traverse newBound (nub qualVars)
    z <- zonk ty
    return (makeForAll ids z)

instantiate (ForAll vars ty) =
  do
    subst <- traverse (\x -> fmap (\t -> (x, t)) newTyVar) vars
    return (apply (fromList subst) ty)
instantiate ty = return ty

subsume scheme1 scheme2@(ForAll _ _) =
  do
    (skolVars, ty) <- skolemise scheme2
    subsume scheme1 ty
    let escVars = skolems scheme1 ++ skolems scheme2
    when (null (including escVars skolVars)) (fail "Escape")
subsume scheme@(ForAll _ _) t2 =
  do
    t1 <- instantiate scheme
    subsume t1 t2
subsume (TypeArrow s1 s2) (TypeArrow s3 s4) =
  do
    subsume s3 s1
    subsume s2 s4
subsume t1 t2 = unify t1 t2

newUniqueName = fmap (makeId . ("t" ++) . show) newUnique

makeForAll tvs1 (ForAll tvs2 ty) =
    makeForAll (tvs1 ++ tvs2) ty
makeForAll tvs ty =
    case nub tvs of
        [] -> ty
        is -> ForAll is ty

skolemise (ForAll vars ty) =
  do
    skolVars <- traverse (const newUniqueName) vars
    let subs = zip vars (fmap SkolemConstant skolVars)
    return (skolVars, apply (fromList subs) ty)
skolemise ty = return ([], ty)

info s = lift (putStrLn s)

writeRef s v = lift (writeIORef s v)

readRef s = lift (readIORef s)

newRef v = lift (newIORef v)

mapKeys f m = fromList (fmap (first f) m)

singleton x y = pure (x, y)
