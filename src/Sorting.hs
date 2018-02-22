module Sorting where

import Syntax
import Data.List(partition)
import Data.Generics.Uniplate.Data(transformBi, para)


{-
Sort let bindings
-}
sortDeclsMod x = (transformBi f . transformBi k) x
  where
    f (LetExpression decls e) =
        LetExpression (sortDecls fromId decls) e
    f e = e

    k (ModuleDeclaration modName decls) =
        ModuleDeclaration modName (sortDecls (qualifyId modName) decls)

sortDecls qual decls =
    let
        -- Top-level definitions have to be qualified with the module name
        getDefs = fmap qual . getDefsD

        -- Only look at the dependency on local variables
        localDefs = foldMap getDefs decls
        getLocalDeps = including localDefs . getDepsD

        -- Remove recursive dependency on itself
        getDeps decl = excluding (getDefs decl) (getLocalDeps decl)
        
        resolved = resolve getDefs getDeps decls
    in resolved

resolve getDefs getDeps vs =
    case partition (null . getDeps) vs of
        ([], []) -> []
        ([], bs) ->
            error ("Cyclic dependency in " ++ pretty (fmap getDeps bs))
        (as, bs) ->
            let
                done = concatMap getDefs as
            in as ++ resolve getDefs (excluding done . getDeps) bs

sortModules = resolve (return . getName . fst) (fmap fst . snd)

{-
Gather variables used in an expression
The second argument of f contains the dependencies of the child expressions
-}
getDepsE e =
    let
        f (Variable v) _ = [v]
        f (ConstructorExpression c) _ = [c]
        f (CaseExpression _ alts) (deps:cs) =
            concat (deps : zipWith (\(p, _) c ->
                excluding (fmap fromId (getDefsP p)) c) alts cs)
        f (LambdaExpression ps _) cs =
            excluding (foldMap (fmap fromId . getDefsP) ps) (concat cs)
        f (LetExpression decls _) cs =
            excluding (foldMap (fmap fromId . getDefsD) decls) (concat cs)
        f _ cs = concat cs
    in para f e

getDepsD (ExpressionDeclaration _ e) = getDepsE e
getDepsD _ = []
