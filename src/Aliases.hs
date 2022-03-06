{-# LANGUAGE OverloadedStrings #-}
module Aliases where

import Syntax
import Data.Generics.Uniplate.Data(rewriteBiM, transformBiM)
import Data.HashMap.Strict(fromList)
import Control.Monad((>=>))
import Control.Applicative(liftA2)


aliasConstructors aliasTable = rewriteBiM f >=> rewriteBiM g >=> rewriteBiM j
  where
    f (ConstructorExpression c) =
        traverse toConstructor (mfind c aliasTable)
    f _ = Right Nothing

    g (ConstructorPattern c ps) =
        traverse (toConstructorPattern ps) (mfind c aliasTable)
    g _ = Right Nothing

    j (TypeConstructor c) =
        traverse Right (mfind c aliasTable)
    j _ = Right Nothing

aliasOperators aliases = transformBiM f >=> transformBiM g >=> transformBiM j
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

    j (TypeInfixOperator a op b) =
        fmap (\al -> makeOpTyp al a b) (findEither op aliases)
    j (TypeConstructor c) | isOperator c =
        fmap TypeConstructor (findEither c aliases)
    j t = Right t

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
        else Left (pretty op
            ++ " with alias " ++ pretty alias
            ++  " is not a constructor")
findConstructor _ op = Right op

captureAliases (ModuleDeclaration _ decls) =
    fromList [(v, alias) | AliasDeclaration v alias <- decls]

captureOperatorAliases (ModuleDeclaration _ decls) =
    fromList [(op, alias) | FixityDeclaration _ _ op alias <- decls]

toConstructor (TypeConstructor c) =
    Right (ConstructorExpression c)
toConstructor other =
    Left ("Cannot convert " ++ pretty other ++ " to a constructor")

toConstructorPattern ps (TypeConstructor c) =
    Right (ConstructorPattern c ps)
toConstructorPattern _ other =
    Left ("Cannot convert " ++ pretty other ++ " to pattern")
