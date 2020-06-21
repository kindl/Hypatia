{-# LANGUAGE OverloadedStrings #-}
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
f =
    let
        fileName = await readFile "Test.txt"
        content = await readFile fileName
    in printText content

is translated to

f = andThen (readFile "Test.txt") (fun fileName ->
  andThen (readFile fileName) (fun content -> printText content))
-}
removeAwaitDeclaration m = transformBi f m
  where
    f (LetExpression decls e) =
        translateAwait decls e
    f e = e

translateAwait decls e = case break isAwaitDecl decls of
    -- If it is a let expression without await
    (otherDecls, []) -> LetExpression otherDecls e
    (otherDecls, ExpressionDeclaration p (AwaitExpression a):rest)
        -> maybeLetExpression otherDecls (makeOp (fromText "andThen") a
            (LambdaExpression [p] (translateAwait rest e)))

-- Don't create empty let expressions
-- when first declaration is AwaitDeclaration
maybeLetExpression [] e = e
maybeLetExpression decls e = LetExpression decls e

isAwaitDecl (ExpressionDeclaration _ (AwaitExpression _)) = True
isAwaitDecl _ = False

{-
Transform a function declaration to expression declaration

foo a b True = a
foo c d False = d

is translated to

foo = (fun v1 v2 v3 -> case (v1, v2, v3) of
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

-- Merges function declarations with the same name
mergeBinds = foldr mergeBindsStep []

mergeBindsStep (id1, ps, e) ((id2, xs):rest) | id1 == id2 =
    (id1, (ps, e):xs):rest
mergeBindsStep (id1, ps, e) acc = (id1, [(ps, e)]):acc

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

toTuplesP1 = foldr1 (makeOpPat (fromText "Tuple"))
toTuplesE1 = foldr1 (makeOp (fromText "Tuple"))
