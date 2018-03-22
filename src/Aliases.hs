module Aliases where

import Syntax
import Data.Generics.Uniplate.Data(transformBi, rewriteBi)
import Data.HashMap.Strict(fromList)
import Control.Arrow(first)


aliasTypes aliasTable = rewriteBi f . rewriteBi g . rewriteBi j
  where
    f (ConstructorExpression c) =
        fmap toConstructor (mfind c aliasTable)
    f _ = Nothing

    g (ConstructorPattern c ps) =
        fmap (toConstructorPattern ps) (mfind c aliasTable)
    g _ = Nothing

    j (TypeConstructor c) =
        mfind c aliasTable
    j _ = Nothing

aliasOperators aliases = transformBi f . transformBi g . transformBi j
  where
    f (PrefixNegation e) =
        FunctionApplication (Variable (fromString "Common.Base.negate")) e  
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

aliasDecls (ModuleDeclaration modName decls) =
    let
        aliases = fromList [(op, alias) |
            FixityDeclaration _ _ op alias <- decls]

        h (ExpressionDeclaration (VariablePattern op) e)
            | isOperator op = ExpressionDeclaration
                (VariablePattern (find op aliases)) e
        h (TypeSignature op t) | isOperator op =
            TypeSignature (find op aliases) t
        h (TypeDeclaration op vars constructors) =
            TypeDeclaration (findConstructor aliases op) vars
                (fmap (first (findConstructor aliases)) constructors)
        h d = d
    in ModuleDeclaration modName (fmap h decls)

findConstructor aliases op | isOperator op =
    let alias = find op aliases in
        if isConstructor alias then alias else
            error (pretty op
                ++ " with alias " ++ pretty alias
                ++  " is not a constructor")
findConstructor _ op = op

captureAliases (ModuleDeclaration modName decls) =
    fromList [(qualifyId modName v, alias)
        | AliasDeclaration v alias <- decls]

captureOperatorAliases (ModuleDeclaration modName decls) =
    fromList [(qualifyId modName op, qualifyId modName alias)
        | FixityDeclaration _ _ op alias <- decls]

toConstructor (TypeConstructor c) = ConstructorExpression c
toConstructor other =
    error ("Cannot convert " ++ pretty other ++ " to a constructor")

toConstructorPattern ps (TypeConstructor c) = ConstructorPattern c ps
toConstructorPattern _ other =
    error ("Cannot convert " ++ pretty other ++ " to pattern")
