module Operators where

import Syntax
import Data.Generics.Uniplate.Data(rewriteBiM)
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
    in rewriteBiM f >=> rewriteBiM g >=> rewriteBiM h

{- Read the fixity declarations -}
captureAssocs (ModuleDeclaration _ _ decls) =
    fromList [(op, (assoc, prec)) | FixityDeclaration assoc prec op _ <- decls]

fixAssocE operatorTable (InfixOperator (InfixOperator e1 child e2) root e3) =
    fixAssocAux operatorTable False InfixOperator e1 child e2 root e3
fixAssocE operatorTable (InfixOperator e1 root (InfixOperator e2 child e3)) =
    fixAssocAux operatorTable True InfixOperator e1 child e2 root e3
fixAssocE _ _ = Right Nothing

fixAssocP operatorTable (PatternInfixOperator (PatternInfixOperator e1 child e2) root e3) =
    fixAssocAux operatorTable False PatternInfixOperator e1 child e2 root e3
fixAssocP operatorTable (PatternInfixOperator e1 root (PatternInfixOperator e2 child e3)) =
    fixAssocAux operatorTable True PatternInfixOperator e1 child e2 root e3
fixAssocP _ _ = Right Nothing

fixAssocT operatorTable (TypeInfixOperator (TypeInfixOperator e1 child e2) root e3) =
    fixAssocAux operatorTable False TypeInfixOperator e1 child e2 root e3
fixAssocT operatorTable (TypeInfixOperator e1 root (TypeInfixOperator e2 child e3)) =
    fixAssocAux operatorTable True TypeInfixOperator e1 child e2 root e3
fixAssocT _ _ = Right Nothing

-- General helper function for patterns, types and expressions
fixAssocAux operatorTable wasRightAssoc constr e1 child e2 root e3 = do
    (assoc1, prec1) <- findEither root operatorTable
    (assoc2, prec2) <- findEither child operatorTable
    let switchedLeft = constr (constr e1 root e2) child e3
    let switchedRight = constr e1 child (constr e2 root e3)
    case (compare prec1 prec2, wasRightAssoc, assoc1, assoc2) of
        (EQ, False, RightAssociative, RightAssociative) -> Right (Just switchedRight)
        (EQ, True, LeftAssociative, LeftAssociative) -> Right (Just switchedLeft)
        (EQ, _, RightAssociative, LeftAssociative) -> fixAssocError root assoc1 child assoc2
        (EQ, _, LeftAssociative, RightAssociative) -> fixAssocError root assoc1 child assoc2
        (GT, False, _, _) -> Right (Just switchedRight)
        (GT, True, _, _) -> Right (Just switchedLeft)
        (_, _, _, _) -> Right Nothing

fixAssocError root assoc1 child assoc2 =
    Left ("Cannot mix operator " ++ renderError root ++ " with " ++ show assoc1 ++ " and "
        ++ renderError child ++ " with " ++ show assoc2)
