module Simplifier where

import Syntax
import Data.List(groupBy, partition)
import Control.Arrow(first)
import Data.Generics.Uniplate.Data(transformBi)

{-
This module translates complicated expressions
to simpler equivalent expressions
-}
simplifications m =
    (splitLambdas . removeFunctionDeclaration) m

splitLambdas m = transformBi f m
  where
    f (LambdaExpression ps e) =
        foldr (\p -> LambdaExpression [p]) e ps
    f e = e

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
    (funDecls, rest) = partition isFunctionDeclaration decls
    merged = fmap mergeBinds (groupBy sameFunDeclName funDecls)
    transformed = fmap transAlt merged
  in rest ++ transformed

isFunctionDeclaration (FunctionDeclaration _ _ _) = True
isFunctionDeclaration _ = False

sameFunDeclName x y = funDeclName x == funDeclName y

funDeclName (FunctionDeclaration x _ _) = x
funDeclName _ = error "funDeclName impossible case"

mergeBinds l =
    (funDeclName (head l), fmap toAlt l)

toAlt (FunctionDeclaration _ ps e) = (ps, e)
toAlt _ = error "toAlt impossible case"

transAlt (name, [(ps, e)]) =
    ExpressionDeclaration (VariablePattern name) (LambdaExpression ps e)
transAlt (name, alts) =
  let
      n = length (fst (head alts))
      vs = nNewVars n
      nalts = fmap (first toTuplesP1) alts
  in
    ExpressionDeclaration (VariablePattern name)
        (LambdaExpression (fmap VariablePattern vs)
            (CaseExpression (toTuplesE1 (fmap (Variable . fromId) vs)) nalts))

toTuplesP1 = foldr1 (makeOpPat (fromString "Tuple"))
toTuplesE1 = foldr1 (makeOp (fromString "Tuple"))
