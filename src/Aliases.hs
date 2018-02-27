module Aliases where

import Syntax
import Data.Generics.Uniplate.Data(transformBi)
import Data.HashMap.Strict(fromList)
import Data.Maybe(fromMaybe)
import Control.Arrow(first)


-- TODO rename modules with aliases
aliasTypes aliasTable = transformBi f . transformBi g . transformBi j
  where
    f e@(ConstructorExpression c) =
        maybe e toConstructor (mfind c aliasTable)
    f e = e

    g p@(ConstructorPattern c ps) =
        maybe p (toConstructorPattern ps) (mfind c aliasTable)
    g p = p

    j t@(TypeConstructor c) =
        fromMaybe t (mfind c aliasTable)
    j t = t

aliasOperators aliases = transformBi f . transformBi g . transformBi j . transformBi h
  where
    f (PrefixNegation e) =
        FunctionApplication (Variable (fromString "Prelude.negate")) e  
    f (InfixOperator a n b) =
        makeOp (find n aliases) a b
    f (Variable x) | isOperator (getId x) =
        Variable (find x aliases)
    f (ConstructorExpression c) | isOperator (getId c) =
        ConstructorExpression (find c aliases)
    f e = e

    g (PatternInfixOperator a n b) =
        makeOpPat (find n aliases) a b
    g (ConstructorPattern c ps) | isOperator (getId c) =
        ConstructorPattern (find c aliases) ps
    g p = p

    j (TypeInfixOperator a n b) =
        makeOpTyp (find n aliases) a b
    j (TypeConstructor c) | isOperator (getId c) =
        TypeConstructor (find c aliases)
    j t = t
    
    h (ModuleDeclaration modName decls) =
        ModuleDeclaration modName (aliasDecls decls)

aliasDecls decls =
    let
        aliases = fromList [(op, alias) | FixityDeclaration _ _ op alias <- decls]

        h (ExpressionDeclaration (VariablePattern op) e) | isOperator op =
            ExpressionDeclaration (VariablePattern (findId op aliases)) e
        h (TypeSignature op t) | isOperator op =
            TypeSignature (findId op aliases) t
        h (TypeDeclaration op vars constructors) =
            TypeDeclaration (findConstructor aliases op) vars
                (fmap (first (findConstructor aliases)) constructors)
        h d = d
    in fmap h decls

findConstructor aliases op | isOperator op =
    let alias = findId op aliases in
        if isConstructor alias then alias else
            error (pretty op ++ " with alias " ++ pretty alias ++  " is not a constructor")
findConstructor _ op = op

captureAliases (ModuleDeclaration modName decls) =
    fromList [(qualifyId modName v, alias) | AliasDeclaration v alias <- decls]

captureOperatorAliases (ModuleDeclaration modName decls) =
    fromList [(qualifyId modName op, qualifyId modName alias) | FixityDeclaration _ _ op alias <- decls]

toConstructor (TypeConstructor c) = ConstructorExpression c
toConstructor other =
    error ("Cannot convert " ++ pretty other ++ " to a constructor")

toConstructorPattern ps (TypeConstructor c) = ConstructorPattern c ps
toConstructorPattern _ other =
    error ("Cannot convert " ++ pretty other ++ " to pattern")
