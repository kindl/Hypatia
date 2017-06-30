module Aliases where

import Syntax
import Data.Generics.Uniplate.Data(transformBi)
import Prelude hiding (lookup)
import Data.HashMap.Strict(lookup, fromList)
import Data.Maybe(fromMaybe)


-- TODO rename modules with aliases
aliasTypes aliasTable = transformBi f . transformBi g . transformBi j
  where
    f e@(ConstructorExpression c) =
        maybe e toConstructor (lookup c aliasTable)
    f e = e

    g p@(ConstructorPattern c ps) =
        maybe p (toConstructorPattern ps) (lookup c aliasTable)
    g p = p

    j t@(TypeConstructor c) =
        fromMaybe t (lookup c aliasTable)
    j t = t

aliasOperators aliasTable = transformBi f . transformBi g . transformBi j
  where
    f (PrefixNegation e) =
        FunctionApplication (Variable (fromString "Prelude.negate")) e  
    f (InfixOperator a n b) =
        makeOp (find n aliasTable) a b
    f (Variable x) | isOperator (getId x) =
        Variable (find x aliasTable)
    f (ConstructorExpression c) | isOperator (getId c) =
        ConstructorExpression (find c aliasTable)
    f e = e

    g (InfixConstructorPattern a n b) =
        makeOpPat (find n aliasTable) a b
    g (ConstructorPattern c ps) | isOperator (getId c) =
        ConstructorPattern (find c aliasTable) ps
    g p = p

    j (TypeInfixOperator a n b) =
        makeOpTyp (find n aliasTable) a b
    j (TypeConstructor c) | isOperator (getId c) =
        TypeConstructor (find c aliasTable)
    j t = t

removeParens m = (transformBi f . transformBi g . transformBi h) m
  where
    f (ParenthesizedExpression e) = e
    f e = e
    
    g (ParenthesizedPattern p) = p
    g p = p
    
    h (ParenthesizedType t) = t
    h t = t

{-
Removes operators without imports

infix + mul
(+) : Number -> Number -> Number
==> plus : Number -> Number -> Number

Function Declaration
infix * mul
a * b = x
==> (*) a b = x

Constructor Pattern
infix * Tuple
a * b = x
==> ((*) a b) = x
-}
aliasDeclarations (ModuleDeclaration modName decls) =
    let
        aliases = fromList [(op, alias) | FixityDeclaration _ _ op alias <- decls]
        
        h (TypeSignature op t) | isOperator op =
            TypeSignature (find op aliases) t
        h d@(ExpressionDeclaration (InfixConstructorPattern p1 op p2) e) | isUnqualified op =
            let alias = find (getId op) aliases
            in if isConstructor alias then d else FunctionDeclaration alias [p1, p2] e
        h (FunctionDeclaration op ps e) | isOperator op =
            let alias = find op aliases
            in if isConstructor alias
                  then error ("Function Declaration with Constructor " ++ pretty op)
                  else FunctionDeclaration alias ps e
        h (EnumDeclaration op vars constructors) | isOperator op =
            let alias = find op aliases
            in if isConstructor alias
                  then EnumDeclaration alias vars (fmap q constructors)
                  else error ("Enum Declaration with Variable " ++ pretty op)
        h (AliasDeclaration op a) | isOperator op =
            let alias = find op aliases
            in if isConstructor alias
                  then AliasDeclaration op a
                  else error ("Alias Declaration with Variable " ++ pretty op)
        h d = d

        q (op, ty) | isOperator op =
            let alias = find op aliases
            in if isConstructor alias
                  then (alias, ty)
                  else error ("Constructor with Variable " ++ pretty op)
        q c = c
    in ModuleDeclaration modName (fmap h decls)


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
