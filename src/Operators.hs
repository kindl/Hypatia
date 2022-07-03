module Operators where

import Syntax
import Data.Generics.Uniplate.Data(transformBiM)
import Data.HashMap.Strict(fromList)
import Control.Monad((>=>))

{-
In the beginning every operator is parsed right associative.
This module changes the operators in the syntax tree
to their specified precedence and associativity

This blog post turned out to be really helpful
http://qfpl.io/posts/quick-and-easy-user-defined-operators/
-}
fixAssoc operatorTable =
    let
        f = fixAssocE operatorTable
        g = fixAssocP operatorTable
        h = fixAssocT operatorTable
    in transformBiM f >=> transformBiM g >=> transformBiM h

{- Read the fixity declarations -}
captureAssocs (ModuleDeclaration _ decls) =
    fromList [(op, (assoc, prec)) | FixityDeclaration assoc prec op _ <- decls]

fixAssocE operatorTable (InfixOperator (InfixOperator e1 child e2) root e3) =
    fixAssocAux operatorTable LeftAssociative InfixOperator e1 child e2 root e3
fixAssocE operatorTable (InfixOperator e1 root (InfixOperator e2 child e3)) =
    fixAssocAux operatorTable RightAssociative InfixOperator e1 child e2 root e3
fixAssocE _ e = Right e

fixAssocP operatorTable (PatternInfixOperator (PatternInfixOperator e1 child e2) root e3) =
    fixAssocAux operatorTable LeftAssociative PatternInfixOperator e1 child e2 root e3
fixAssocP operatorTable (PatternInfixOperator e1 root (PatternInfixOperator e2 child e3)) =
    fixAssocAux operatorTable RightAssociative PatternInfixOperator e1 child e2 root e3
fixAssocP _ e = Right e

fixAssocT operatorTable (TypeInfixOperator (TypeInfixOperator e1 child e2) root e3) =
    fixAssocAux operatorTable LeftAssociative TypeInfixOperator e1 child e2 root e3
fixAssocT operatorTable (TypeInfixOperator e1 root (TypeInfixOperator e2 child e3)) =
    fixAssocAux operatorTable RightAssociative TypeInfixOperator e1 child e2 root e3
fixAssocT _ e = Right e

-- General helper function for patterns, types and expressions
fixAssocAux operatorTable prevAssoc constr e1 child e2 root e3 = do
    (assoc1, prec1) <- findEither root operatorTable
    (assoc2, prec2) <- findEither child operatorTable
    let left = constr (constr e1 root e2) child e3
    let right = constr e1 child (constr e2 root e3)
    case (compare prec1 prec2, prevAssoc, assoc1, assoc2) of
        (EQ, LeftAssociative, RightAssociative, RightAssociative) -> Right right
        (EQ, RightAssociative, LeftAssociative, LeftAssociative) -> Right left
        (EQ, _, RightAssociative, LeftAssociative) -> fixAssocError child root
        (EQ, _, LeftAssociative, RightAssociative) -> fixAssocError child root
        (GT, LeftAssociative, _, _) -> Right right
        (GT, RightAssociative, _, _) -> Right left
        (_, LeftAssociative, _, _) -> Right left
        (_, RightAssociative, _, _) -> Right right
        _ -> fixAssocError child root

fixAssocError child root =
    Left ("Cannot mix operator " ++ renderError child ++ " and " ++ renderError root)
