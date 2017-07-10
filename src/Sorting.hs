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
Gather variables used in an expression
-}
getDepsE e =
    let
        f (Variable v) _ = [v]
        f (ConstructorExpression c) _ = [c]
        f (CaseLambdaExpression alts) cs =
            concat (zipWith (\(p, _) c -> excluding (fmap fromId (getDefsP p)) c) alts cs)
        f (LambdaExpression ps _) cs =
            excluding (foldMap (fmap fromId . getDefsP) ps) (concat cs)
        f (LetExpression decls _) cs =
            excluding (foldMap (fmap fromId . getDefsD) decls) (concat cs)
        f _ cs = concat cs
    in para f e

getDepsD (ExpressionDeclaration _ e) = getDepsE e
getDepsD _ = []
