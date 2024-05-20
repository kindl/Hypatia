module Sorting where

import Syntax
import Data.List(partition)
import Data.Generics.Uniplate.Data(para)
import Data.Foldable(foldMap')
import qualified Data.HashSet as Set


{-
Sort declarations in modules, so that order of declarations does not matter.
This is like a topological ordering, meaning declarations that other declarations depend on,
will be moved to the beginning.

This could also be enabled for let-expressions as follows:
```
sortDeclsMod (ModuleDeclaration modName imports decls) =
    let
        f (LetExpression ds e) =
            fmap (flip LetExpression e) (sortDecls ds)
        f e = Right e
    in transformBiM f =<< fmap (ModuleDeclaration modName imports) (sortDecls decls)
```

However, to keep let-expressions simple and predictable, this behaviour was disabled.
-}

sortDeclsMod (ModuleDeclaration modName imports decls) =
    fmap (ModuleDeclaration modName imports) (sortDecls decls)

sortDecls decls =
    let
        -- Only look at the dependency on local variables
        localDefs = foldMap' getDefsD decls
        getLocalDeps x = Set.intersection localDefs (getDepsD x)

        -- Remove recursive dependency on itself
        getDeps decl = Set.difference (getLocalDeps decl) (getDefsD decl)
    in resolve getDefsD getDeps mempty decls

resolve getDefs getDeps done rest =
    case partition (null . flip Set.difference done . getDeps) rest of
        ([], []) ->
            Right mempty
        ([], ys) ->
            Left ("Cyclic dependency in " ++ foldMap' (renderSetToError . getDeps) ys)
        (xs, ys) -> do
            -- in the first run, xs contains all type signatures
            -- they have no value dependencies but define an id
            -- here these ids get marked as done which allows cycles
            let doneDefs = foldMap' getDefs xs
            next <- resolve getDefs getDeps (doneDefs <> done) ys
            Right (xs <> next)

-- Sort imported modules, so that modules that other modules depend on are compiled first
sortModules =
    resolve (Set.singleton . getName) importedModules mempty


-- Gather variables used in an expression.
-- The second argument of f contains the dependencies of the child expressions
getDepsE e =
    let
        f (Variable v) _ = Set.singleton v
        f (ConstructorExpression c) _ = Set.singleton c
        f (CaseExpression _ alts) (deps:cs) =
            mconcat (deps : zipWith (\(p, _) c ->
                Set.difference c (getDefsP p)) alts cs)
        f (LambdaExpression ps _) cs =
            Set.difference (mconcat cs) (foldMap' getDefsP ps)
        f (LetExpression decls _) cs =
            Set.difference (mconcat cs) (foldMap' getDefsD decls)
        f _ cs = mconcat cs
    in para f e

getDepsD (ExpressionDeclaration _ e) =
    getDepsE e
getDepsD (FunctionDeclaration _ alts) =
    foldMap' (\(ps, e) -> Set.difference (getDepsE e) (foldMap' getDefsP ps)) alts
getDepsD _ =
    mempty

getDefsP p = Set.fromList (getBindings p)

-- Type signatures are not considered for sorting
getDefsD (ExpressionDeclaration p _) = getDefsP p
getDefsD (FunctionDeclaration v _) = Set.singleton v
getDefsD (TypeDeclaration _ _ cs) = foldMap' (Set.singleton . fst) cs
getDefsD _ = mempty
