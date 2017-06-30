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
    g (InfixConstructorPattern a o1 (InfixConstructorPattern b o2 c)) =
        fixAssocO operatorTable InfixConstructorPattern a o1 b o2 c
    g p = p

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
--TODO prec1 == prec2 has to be an error if they don't have the same associativity
fixAssocO operatorTable constr f1 o1 fe1 o2 fe2 =
    let
        (assoc1, prec1) = find o1 operatorTable
        (assoc2, prec2) = find o2 operatorTable
    in if o1 == o2
        then (case assoc1 of
            LeftAssociative -> constr (constr f1 o1 fe1) o1 fe2
            RightAssociative -> constr f1 o1 (constr fe1 o1 fe2)
            None -> error ("Operator " ++ pretty o1 ++ " has no associativity"))
        else if prec1 > prec2
            then constr (constr f1 o1 fe1) o2 fe2
            else constr f1 o1 (constr fe1 o2 fe2)
