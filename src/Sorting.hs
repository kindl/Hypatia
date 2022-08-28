module Sorting where

import Syntax
import Data.List(partition)
import Data.Generics.Uniplate.Data(transformBiM, para)
import Data.Foldable(foldMap')


-- Sorting of declarations in let bindings and modules
sortDeclsMod (ModuleDeclaration modName imports decls) =
    let
        f (LetExpression ds e) =
            fmap (flip LetExpression e) (sortDecls ds)
        f e = Right e
    in transformBiM f =<< fmap (ModuleDeclaration modName imports) (sortDecls decls)
        
sortDecls decls =
    let
        -- Only look at the dependency on local variables
        localDefs = foldMap' getDefsD decls
        getLocalDeps = including localDefs . getDepsD

        -- Remove recursive dependency on itself
        getDeps decl = excluding (getDefsD decl) (getLocalDeps decl)
    in resolve getDefsD getDeps [] decls

resolve getDefs getDeps done rest =
    case partition (null . excluding done . getDeps) rest of
        ([], []) ->
            Right []
        ([], ys) ->
            Left ("Cyclic dependency in " ++ renderError (fmap getDeps ys))
        (xs, ys) -> do
            -- in the first run, xs contains all type signatures
            -- they have no value dependencies but define an id
            -- here these ids get marked as done which allows cycles
            let doneDefs = foldMap' getDefs xs
            next <- resolve getDefs getDeps (doneDefs ++ done) ys
            Right (xs ++ next)

sortModules = resolve (return . getName) gatherImports []


--Gather variables used in an expression
--The second argument of f contains
--the dependencies of the child expressions
getDepsE e =
    let
        f (Variable v) _ = [v]
        f (ConstructorExpression c) _ = [c]
        f (CaseExpression _ alts) (deps:cs) =
            concat (deps : zipWith (\(p, _) c ->
                excluding (getDefsP p) c) alts cs)
        f (LambdaExpression ps _) cs =
            excluding (foldMap' getDefsP ps)
                (concat cs)
        f (LetExpression decls _) cs =
            excluding (foldMap' getDefsD decls)
                (concat cs)
        f _ cs = concat cs
    in para f e

getDepsD (ExpressionDeclaration _ e) = getDepsE e
getDepsD _ = []
