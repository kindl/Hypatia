{-# LANGUAGE OverloadedStrings #-}
module Simplifier where

import Syntax
import Data.Generics.Uniplate.Data(transformBi)


-- This module translates complicated expressions
-- to simpler equivalent expressions

-- Note that the simplifications run right to left and the order matters.
simplifications m =
    (curryLambdas . removeAwaitDeclaration . mergeFunctionDeclarations) m

curryLambdas m =
    let
        f (LambdaExpression ps e) = curryLambda ps e
        f e = e
    in transformBi f m

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
        fileEnding = ".txt"
        fileName = await readFile (filePath ++ fileEnding)
        content = await readFile fileName
    in printText content

is translated to

f = let
        filePath = "Test"
        fileEnding ".txt"
    in bind (readFile (filePath ++ fileEnding)) (fun fileName ->
        bind (readFile fileName) (fun content -> printText content))
-}
removeAwaitDeclaration m = transformBi f m
  where
    f (LetExpression decls e) =
        translateAwait (splitAwaitDecls decls) e
    f e = e

translateAwait [] e = e
translateAwait (Right ds:rest) e = LetExpression ds (translateAwait rest e)
translateAwait (Left (p, a):rest) e = makeOpApp (fromText "bind") a
    (LambdaExpression [p] (translateAwait rest e))

-- Put normal declarations into groups and break on declarations with await
splitAwaitDecls :: [Declaration] -> [Either (Pattern, Expression) [Declaration]]
splitAwaitDecls = foldr splitAwaitDeclsStep []

splitAwaitDeclsStep (ExpressionDeclaration p (AwaitExpression a)) acc =
    Left (p, a):acc
splitAwaitDeclsStep d (Right ds:rest) = Right (d:ds):rest
splitAwaitDeclsStep d acc = Right [d]:acc


{-
Merge function declarations.
A function declaration like this is first parsed seperately
```
index 0 (Element e _) = e
index n (Element _ es) = index (n - 1) es
```
and is merged like this
```
index
    0 (Element e _) = e
    n (Element _ es) = index (n - 1) es
```
-}
mergeFunctionDeclarations m =
    let
        f (LetExpression decls e) =
            LetExpression (groupBinds decls) e
        f e = e

        k (ModuleDeclaration name imports decls) =
            ModuleDeclaration name imports (groupBinds decls)
    in transformBi f (k m)

groupBinds decls =
    let
        merged = foldr groupBindsStep [] decls
        transformed = fmap (fmap (uncurry FunctionDeclaration)) merged
    in fmap (either id id) transformed

groupBindsStep (FunctionDeclaration id1 [(ps, e)]) (Right (id2, xs):rest) | id1 == id2 =
    Right (id1, (ps, e):xs):rest
groupBindsStep (FunctionDeclaration id1 alts) acc =
    Right (id1, alts):acc
groupBindsStep other acc =
    Left other:acc
