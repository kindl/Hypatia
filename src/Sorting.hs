module Sorting where

import Syntax
import Data.List(partition)
import Data.Generics.Uniplate.Data(transformBi)


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

sortDecls fQual decls =
    let
        triples = fmap (second (fmap fQual) . defPair) decls
        defs = foldMap (fst . snd) triples
        locals = keepDeps defs triples
    in sortPairs locals

defPair decl =
    (decl, (getDefsD decl, getDepsD decl))

removeDeps xs =
    fmap (third (excluding xs))

keepDeps xs =
    fmap (third (including xs))

second f (a, (b, c)) = (a, (f b, c))

third f (a, (b, c)) = (a, (b, f c))

isDepless (_, (defs, deps)) = null (excluding defs deps)

sortPairs pairs =
    case partition isDepless pairs of
         ([], []) -> []
         ([], xs) ->
            error ("Possible mutual recursion " ++ pretty (fmap snd xs))
         (withoutDep, withDep) ->
              let
                defs = foldMap (fst . snd) withoutDep
                rest = removeDeps defs withDep
                decls = fmap fst withoutDep
              in decls ++ sortPairs rest

{-
Gather variables are used in an expression
-}
getDeps (Variable v) = [v]
getDeps (ConstructorExpression c) = [c]
getDeps (FunctionApplication f e) = getDeps f ++ getDeps e
getDeps (CaseLambdaExpression alts) =
    foldMap (uncurry getDepsAlt) alts
getDeps (LambdaExpression [p] e) = getDepsAlt p e
getDeps (LetExpression decls e) =
    let
        defs = foldMap getDefsD decls
        deps = foldMap getDepsD decls ++ getDeps e
    in excluding (fmap fromId defs) deps
getDeps (IfExpression c th el) =
    getDeps c ++ getDeps th ++ getDeps el
getDeps _ = []

getDepsAlt p e = excluding (fmap fromId (getDefsP p)) (getDeps e)

getDepsD (ExpressionDeclaration _ e) = getDeps e
getDepsD _ = []
