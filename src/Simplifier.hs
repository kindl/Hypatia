{-# LANGUAGE OverloadedStrings #-}
module Simplifier where

import Syntax
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
Transform declarations with await into bind expressions

f = let
        filePath = "Test"
        fileEnding ".txt"
        fileName = await readFile (filePath & fileEnding)
        content = await readFile fileName
    in printText content

is translated to

f = let
        filePath = "Test"
        fileEnding ".txt"
    in bind (readFile (filePath & fileEnding)) (fun fileName ->
        bind (readFile fileName) (fun content -> printText content))
-}
removeAwaitDeclaration m = transformBi f m
  where
    f (LetExpression decls e) =
        translateAwait (splitAwaitDecls decls) e
    f e = e

translateAwait [] e = e
translateAwait (Right ds:rest) e = LetExpression ds (translateAwait rest e)
translateAwait (Left (p, a):rest) e = makeOp (fromText "bind") a
    (LambdaExpression [p] (translateAwait rest e))

-- Put normal declarations into groups and break on declarations with await
splitAwaitDecls :: [Declaration] -> [Either (Pattern, Expression) [Declaration]]
splitAwaitDecls = foldr splitAwaitDeclsStep []

splitAwaitDeclsStep (ExpressionDeclaration p (AwaitExpression a)) acc =
    Left (p, a):acc
splitAwaitDeclsStep d (Right ds:rest) = Right (d:ds):rest
splitAwaitDeclsStep d acc = Right [d]:acc


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
        merged = groupBinds decls
        transformed = fmap (fmap transAlt) merged
    in fmap (either id id) transformed


-- Groups function declarations with the same name
groupBinds :: [Declaration] -> [Either Declaration (Id, [([Pattern], Expression)])]
groupBinds = foldr groupBindsStep []

groupBindsStep (FunctionDeclaration id1 ps e) (Right (id2, xs):rest) | id1 == id2 =
    Right (id1, (ps, e):xs):rest
groupBindsStep (FunctionDeclaration id1 ps e) acc = Right (id1, [(ps, e)]):acc
groupBindsStep other acc = Left other:acc

transAlt (name, [(ps, e)]) =
    ExpressionDeclaration (VariablePattern name) (LambdaExpression ps e)
transAlt (name, alts) =
    let
        n = length (fst (head alts))
        vs = nNewVars n
        nalts = fmap (first toTuplesP1) alts
        e = toTuplesE1 (fmap (Variable . fromId) vs)
    in ExpressionDeclaration (VariablePattern name)
        (LambdaExpression (fmap VariablePattern vs)
            (CaseExpression e nalts))

toTuplesP1 = foldr1 (makeOpPat (fromText "Tuple"))

toTuplesE1 = foldr1 (makeOp (fromText "Tuple"))
