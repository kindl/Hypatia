module Simplifier where

import Syntax
import Data.Either(partitionEithers)
import Control.Arrow(first)
import Data.Generics.Uniplate.Data(transformBi)

{-
This module translates complicated expressions
to simpler equivalent expressions
-}
splitLambdas m = transformBi f m
  where
    f (LambdaExpression ps e) =
        foldr (\p -> LambdaExpression [p]) e ps
    f e = e


removeParens m = (transformBi f . transformBi g . transformBi h) m
  where
    f (ParenthesizedExpression e) = e
    f e = e
    
    g (ParenthesizedPattern p) = p
    g p = p
    
    h (ParenthesizedType t) = t
    h t = t


{-
transform function declaration to expression declaration
bla a b True = a
bla c d False = d
==>
bla = (fun v1 v2 v3 -> case (v1, v2, v3) of
    (a, b, True) -> a
    (c, d, False) -> d)

-}    
removeFunctionDeclaration m = (transformBi f . transformBi k) m
  where
    f (LetExpression decls e) =
        LetExpression (transBinds decls) e
    f e = e
    
    k (ModuleDeclaration name decls) =
        ModuleDeclaration name (transBinds decls)

transBinds decls =
  let
    splitted = fmap fromFunctionDeclaration decls
    (funDecls, rest) = partitionEithers splitted
    merged = mergeBinds funDecls
    transformed = fmap transAlt merged
  in rest ++ transformed

fromFunctionDeclaration (FunctionDeclaration n ps e) = Left (n, ps, e)
fromFunctionDeclaration e = Right e

mergeBinds = foldr mergeBindsAcc []

mergeBindsAcc (ident, ps, e) acc =
    case acc of
        ((ident2, xs):rest) | ident == ident2 ->
            (ident, ((ps, e):xs)):rest
        _ -> (ident, [(ps, e)]):acc

transAlt (name, [(ps, e)]) =
    ExpressionDeclaration (VariablePattern name) (LambdaExpression ps e)
transAlt (name, alts) =
  let
      n = length (fst (head alts))
      vs = nNewVars n
      nalts = fmap (first toTuplesP1) alts
      e = toTuplesE1 (fmap (Variable . fromId) vs)
  in
    ExpressionDeclaration (VariablePattern name)
        (LambdaExpression (fmap VariablePattern vs)
            (CaseExpression e nalts))

toTuplesP1 = foldr1 (makeOpPat (fromString "Tuple"))
toTuplesE1 = foldr1 (makeOp (fromString "Tuple"))
