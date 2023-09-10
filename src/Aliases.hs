{-# LANGUAGE OverloadedStrings #-}
module Aliases where

import Syntax
import Data.Generics.Uniplate.Data(transformBiM)
import Data.HashMap.Strict(fromList)
import Control.Monad((>=>))
import Control.Applicative(liftA2)


aliasConstructors aliasTable =
    let
        f e@(ConstructorExpression c) =
            findConstructor c aliasTable <> Right e
        f e = Right e

        g e@(ConstructorPattern _ c ps) =
            findConstructorPattern ps c aliasTable <> Right e
        g e = Right e

        h e@(TypeConstructor c) =
            findTypeConstructor c aliasTable <> Right e
        h e = Right e
    in transformBiM f >=> transformBiM g >=> transformBiM h

aliasOperators aliases =
    let
        f (PrefixNegation e) =
            Right (FunctionApplication (Variable (fromText "Native.negate")) e)
        f (InfixOperator a op b) =
            fmap (\al -> makeOp al a b) (findAlias op aliases)
        f (Variable x) | isOperator x =
            fmap Variable (findAlias x aliases)
        f (ConstructorExpression c) | isOperator c =
            fmap ConstructorExpression (findAlias c aliases)
        f e = Right e

        g (PatternInfixOperator a op b) =
            fmap (\al -> makeOpPat al a b) (findAlias op aliases)
        g (ConstructorPattern info c ps) | isOperator c =
            fmap (\op -> ConstructorPattern info op ps) (findAlias c aliases)
        g p = Right p

        h (TypeInfixOperator a op b) =
            fmap (\al -> makeOpTyp al a b) (findAlias op aliases)
        h (TypeConstructor c) | isOperator c =
            fmap TypeConstructor (findAlias c aliases)
        h t = Right t
    in transformBiM f >=> transformBiM g >=> transformBiM h


aliasOperatorsMod (ModuleDeclaration modName imports decls) =
    let
        aliases = fromList [(op, alias) |
            FixityDeclaration _ _ op alias <- decls]

        k (FunctionDeclaration op alts) | isOperator op =
            fmap (\al -> FunctionDeclaration al alts) (findAlias op aliases)
        k (ExpressionDeclaration (VariablePattern op) e) | isOperator op =
            fmap (\al -> ExpressionDeclaration (VariablePattern al) e) (findAlias op aliases)
        k (TypeSignature op t) | isOperator op =
            fmap (flip TypeSignature t) (findAlias op aliases)
        k (TypeDeclaration op vars constructors) =
            liftA2 (\aliases' constructors' -> TypeDeclaration aliases' vars constructors')
                (findConstructorOp aliases op)
                (traverse (firstA (findConstructorOp aliases)) constructors)
        k d = Right d
    in fmap (ModuleDeclaration modName imports) (traverse k decls)

findConstructorOp aliases op | isOperator op = do
    alias <- findAlias op aliases
    if isConstructor alias
        then Right alias
        else Left (renderError op
            ++ " with alias " ++ renderError alias
            ++  " is not a constructor")
findConstructorOp _ op = Right op

captureAliases (ModuleDeclaration _ _ decls) =
    fromList [(v, alias) | AliasDeclaration v alias <- decls]

captureOperatorAliases (ModuleDeclaration _ _ decls) =
    fromList [(op, alias) | FixityDeclaration _ _ op alias <- decls]

findConstructor c aliasTable = do
    found <- findConstructorAlias c aliasTable
    return (ConstructorExpression found)

findTypeConstructor c aliasTable = do
    found <- findConstructorAlias c aliasTable
    return (TypeConstructor found)

findConstructorPattern ps c aliasTable = do
    found <- findConstructorAlias c aliasTable
    return (ConstructorPattern TaggedRepresentation found ps)

findConstructorAlias c aliasTable =  do
    found <- findEither c aliasTable
    tc <- unpackTypeConstructor found
    return (switchWithLocation c tc)

unpackTypeConstructor (TypeConstructor c) = Right c
unpackTypeConstructor other = Left ("Cannot convert " ++ renderError other ++ " to a constructor")

-- finds alias e.g. eq for == but preserves location info
findAlias name m = do
    alias <- findEither name m
    return (switchWithLocation name alias)

switchWithLocation original (Name qs (Id i _)) =
    let originalLocation = getLocation (getId original)
    in Name qs (Id i originalLocation)
