{-# LANGUAGE OverloadedStrings #-}
module Aliases where

import Syntax
import Data.Generics.Uniplate.Data(transformBiM)
import Data.HashMap.Strict(fromList)
import Control.Monad((>=>))
import Control.Applicative(liftA2)


aliasConstructors aliasTable = transformBiM f >=> transformBiM g >=> transformBiM h
  where
    f e@(ConstructorExpression c) =
        (findEither c aliasTable >>= toConstructor) <> Right e
    f e = Right e

    g e@(ConstructorPattern c ps) =
        (findEither c aliasTable >>= toConstructorPattern ps) <> Right e
    g e = Right e

    h e@(TypeConstructor c) =
        findEither c aliasTable <> Right e
    h e = Right e

aliasOperators aliases = transformBiM f >=> transformBiM g >=> transformBiM h
  where
    f (PrefixNegation e) =
        Right (FunctionApplication (Variable (fromText "Native.negate")) e)
    f (InfixOperator a op b) =
        fmap (\al -> makeOp al a b) (findEither op aliases)
    f (Variable x) | isOperator x =
        fmap Variable (findEither x aliases)
    f (ConstructorExpression c) | isOperator c =
        fmap ConstructorExpression (findEither c aliases)
    f e = Right e

    g (PatternInfixOperator a op b) =
        fmap (\al -> makeOpPat al a b) (findEither op aliases)
    g (ConstructorPattern c ps) | isOperator c =
        fmap (flip ConstructorPattern ps) (findEither c aliases)
    g p = Right p

    h (TypeInfixOperator a op b) =
        fmap (\al -> makeOpTyp al a b) (findEither op aliases)
    h (TypeConstructor c) | isOperator c =
        fmap TypeConstructor (findEither c aliases)
    h t = Right t

aliasOperatorsMod (ModuleDeclaration modName decls) =
    let
        aliases = fromList [(op, alias) |
            FixityDeclaration _ _ op alias <- decls]

        h (ExpressionDeclaration (VariablePattern op) e) | isOperator op =
            fmap (\al -> ExpressionDeclaration (VariablePattern al) e) (findEither op aliases)
        h (TypeSignature op t) | isOperator op =
            fmap (flip TypeSignature t) (findEither op aliases)
        h (TypeDeclaration op vars constructors) =
            liftA2 (\aliases' constructors' -> TypeDeclaration aliases' vars constructors')
                (findConstructor aliases op)
                (traverse (firstA (findConstructor aliases)) constructors)
        h d = Right d
    in fmap (ModuleDeclaration modName) (traverse h decls)

findConstructor aliases op | isOperator op = do
    alias <- findEither op aliases
    if isConstructor alias
        then Right alias
        else Left (renderError op
            ++ " with alias " ++ renderError alias
            ++  " is not a constructor")
findConstructor _ op = Right op

captureAliases (ModuleDeclaration _ decls) =
    fromList [(v, alias) | AliasDeclaration v alias <- decls]

captureOperatorAliases (ModuleDeclaration _ decls) =
    fromList [(op, alias) | FixityDeclaration _ _ op alias <- decls]

toConstructor (TypeConstructor c) =
    Right (ConstructorExpression c)
toConstructor other =
    Left ("Cannot convert " ++ renderError other ++ " to a constructor")

toConstructorPattern ps (TypeConstructor c) =
    Right (ConstructorPattern c ps)
toConstructorPattern _ other =
    Left ("Cannot convert " ++ renderError other ++ " to pattern")
