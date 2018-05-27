module Operators where

import Syntax
import Data.Generics.Uniplate.Data(transformBi, rewrite)
import Data.HashMap.Strict(fromList)


{-
In the beginning every operator is parsed right associative.
This module changes the operators in the syntax tree
to their specified precedence and associativity

This blog post turned out to be really helpful
http://qfpl.io/posts/quick-and-easy-user-defined-operators/
-}
fixAssoc operatorTable =
    let
        f = rewrite (fixAssocE operatorTable)
        g = rewrite (fixAssocP operatorTable)
        h = rewrite (fixAssocT operatorTable)
    in transformBi f . transformBi g . transformBi h

{- Read the fixity declarations -}
captureAssocs (ModuleDeclaration modName decls) =
    fromList [(qualifyId modName op, (assoc, prec)) | FixityDeclaration assoc prec op _ <- decls]

fixAssocE operatorTable (InfixOperator (InfixOperator e1 child e2) root e3) =
    fixAssocAux operatorTable LeftAssociative InfixOperator e1 child e2 root e3
fixAssocE operatorTable (InfixOperator e1 root (InfixOperator e2 child e3)) =
    fixAssocAux operatorTable RightAssociative InfixOperator e1 child e2 root e3
fixAssocE _ _ = Nothing

fixAssocP operatorTable (PatternInfixOperator (PatternInfixOperator e1 child e2) root e3) =
    fixAssocAux operatorTable LeftAssociative PatternInfixOperator e1 child e2 root e3
fixAssocP operatorTable (PatternInfixOperator e1 root (PatternInfixOperator e2 child e3)) =
    fixAssocAux operatorTable RightAssociative PatternInfixOperator e1 child e2 root e3
fixAssocP _ _ = Nothing

fixAssocT operatorTable (TypeInfixOperator (TypeInfixOperator e1 child e2) root e3) =
    fixAssocAux operatorTable LeftAssociative TypeInfixOperator e1 child e2 root e3
fixAssocT operatorTable (TypeInfixOperator e1 root (TypeInfixOperator e2 child e3)) =
    fixAssocAux operatorTable RightAssociative TypeInfixOperator e1 child e2 root e3
fixAssocT _ _ = Nothing

-- General helper function for patterns, types and expressions
fixAssocAux operatorTable prevAssoc constr e1 child e2 root e3 =
    let
        (assoc1, prec1) = find root operatorTable
        (assoc2, prec2) = find child operatorTable
        left = constr (constr e1 root e2) child e3
        right = constr e1 child (constr e2 root e3)
    in case (compare prec1 prec2, prevAssoc, assoc1, assoc2) of
        (EQ, LeftAssociative, RightAssociative, RightAssociative) -> Just right
        (EQ, RightAssociative, LeftAssociative, LeftAssociative) -> Just left
        (EQ, _, RightAssociative, LeftAssociative) -> fixAssocError child root
        (EQ, _, LeftAssociative, RightAssociative) -> fixAssocError child root
        (GT, LeftAssociative, _, _) -> Just right
        (GT, RightAssociative, _, _) -> Just left
        _ -> Nothing

fixAssocError child root =
    error ("Cannot mix operator " ++ pretty child ++ " and " ++ pretty root)
