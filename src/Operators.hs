module Operators where

import Syntax
import Data.Generics.Uniplate.Data(transformBi)
import Data.HashMap.Strict(fromList)


{-
In the beginning we parse everything
right-associative and without precedence.

This module changes the syntax tree
to the specified precedence and associativity
-}
fixAssoc operatorTable =
  let
    g (PatternInfixOperator a o1 (PatternInfixOperator b o2 c)) =
        fixAssocO operatorTable PatternInfixOperator a o1 b o2 c
    g p = p

    f (InfixOperator (InfixOperator a o1 b) o2 c) =
        fixAssocO operatorTable InfixOperator a o1 b o2 c
    f (InfixOperator a o1 (InfixOperator b o2 c)) =
        fixAssocO operatorTable InfixOperator a o1 b o2 c
    f e = e

    h (TypeInfixOperator a o1 (TypeInfixOperator b o2 c)) =
        fixAssocO operatorTable TypeInfixOperator a o1 b o2 c
    h e = e
  in transformBi f . transformBi g . transformBi h

{- Gather information -}
-- Read the fixity declarations from the tree
captureAssocs (ModuleDeclaration modName decls) =
    fromList [(qualifyId modName op, (assoc, prec)) | FixityDeclaration assoc prec op _ <- decls]

-- General helper function for patterns and expressions
-- TODO 2 * 3 + 5 * 7 + 11 * 13
fixAssocO operatorTable constr f1 o1 fe1 o2 fe2 =
    let
        (assoc1, prec1) = find o1 operatorTable
        (assoc2, prec2) = find o2 operatorTable
    in case compare prec1 prec2 of
        LT -> constr f1 o1 (constr fe1 o2 fe2)
        GT -> constr (constr f1 o1 fe1) o2 fe2
        EQ -> case (assoc1, assoc2) of
            (LeftAssociative, LeftAssociative) -> constr (constr f1 o1 fe1) o2 fe2
            -- TODO this is probably an ambiguous parse?
            (LeftAssociative, RightAssociative) -> constr f1 o1 (constr fe1 o2 fe2)
            (RightAssociative, LeftAssociative) -> constr (constr f1 o1 fe1) o2 fe2
            (RightAssociative, RightAssociative) -> constr f1 o1 (constr fe1 o2 fe2)
            _ -> error ("Operator " ++ pretty o1 ++ " has no associativity")
