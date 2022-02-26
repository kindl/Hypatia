module Qualification where

import Syntax
import Data.Generics.Uniplate.Data(transform, descend, transformBi)
import Data.Foldable(foldMap')
import Control.Arrow(first)


-- Qualification is split into value level and type level
-- otherwise type Unit = Unit would be ambiguous

-- Value Level
qualifyNames quals (ModuleDeclaration name decls) =
    ModuleDeclaration name (fmap (qualifyD quals) decls)

-- Qualify the bindings and expressions appearing in declarations
qualifyD quals (ExpressionDeclaration p e) =
    ExpressionDeclaration (qualifyP quals p) (qualifyE quals e)
qualifyD quals (TypeDeclaration v vars cs) =
    TypeDeclaration v vars (fmap (first (findName quals)) cs)
qualifyD quals (TypeSignature v t) =
    TypeSignature (findName quals v) t
qualifyD quals (AliasDeclaration v t) =
    AliasDeclaration (findName quals v) t
-- TODO make the following also work on type level
-- For example, `infixl 7 * Tuple` Tuple will not be in quals
qualifyD quals (FixityDeclaration a p op alias) =
    FixityDeclaration a p (findName quals op) (findName quals alias)
qualifyD _ decl@(ImportDeclaration _ _ _) = decl
qualifyD _ decl = error ("Bug: Unexpected declaration " ++ show decl)


qualifyP quals pat =
    let
        f (VariablePattern v) =
            VariablePattern (findName quals v)
        f (AliasPattern v p) =
            AliasPattern (findName quals v) p
        f (ConstructorPattern c ps) =
            ConstructorPattern (findName quals c) ps
        f (PatternInfixOperator p1 op p2) =
            PatternInfixOperator p1 (findName quals op) p2
        f p = p
    in transform f pat


qualifyE quals (Variable n) =
    Variable (findName quals n)
qualifyE quals (ConstructorExpression c) =
    ConstructorExpression (findName quals c)
qualifyE quals (LetExpression decls e) =
    let
        locals = toLocals (foldMap' captureNameD decls)
        newQuals = unionUnique locals quals
    in LetExpression (fmap (qualifyD newQuals) decls)
        (qualifyE newQuals e)
qualifyE quals (CaseExpression e alts) =
    CaseExpression (qualifyE quals e) (fmap (qualifyA quals) alts)
qualifyE quals (LambdaExpression [p] e) =
    let (qp, qe) = qualifyA quals (p, e)
    in LambdaExpression [qp] qe
qualifyE quals (InfixOperator ea name eb) =
    InfixOperator (qualifyE quals ea)
        (findName quals name) (qualifyE quals eb)
qualifyE quals e =
    descend (qualifyE quals) e

qualifyA quals (p, e) =
    let
        locals = toLocals (getDefsP p)
        newQuals = unionUnique locals quals
    in (qualifyP newQuals p, qualifyE newQuals e)


captureNames (ModuleDeclaration modName decls) = mappend
    (toQualifieds modName (foldMap' captureNameD decls))
    (toQualifieds modName (foldMap' captureTopName decls))

captureNameD (ExpressionDeclaration p _) = getDefsP p
captureNameD _ = []

-- Type signatures can also appear in normal declarations,
-- but then they need an expression declaration
-- and are captured there
captureTopName (TypeSignature identifier _) = [identifier]
captureTopName (TypeDeclaration _ _ cs) = fmap fst cs
captureTopName (AliasDeclaration identifier _) = [identifier]
captureTopName (FixityDeclaration _ _ op _) = [op]
captureTopName _ = []


-- Type Level
qualifyTypeNames quals (ModuleDeclaration name decls) =
    let
        f (TypeInfixOperator ta op tb) =
            TypeInfixOperator ta (findName quals op) tb
        f (TypeConstructor c) =
            TypeConstructor (findName quals c)
        f t = t
    in transformBi f (ModuleDeclaration name (fmap (qualifyTypesD quals) decls))

qualifyTypesD quals (TypeDeclaration v vs cs) = TypeDeclaration (findName quals v) vs cs
qualifyTypesD _ decl = decl

captureTypeNames (ModuleDeclaration modName decls) =
    toQualifieds modName (foldMap' captureTypeNameD decls)

captureTypeNameD (AliasDeclaration identifier _) = [identifier]
captureTypeNameD (TypeDeclaration identifier _ _) = [identifier]
captureTypeNameD _ = []

findName quals (Name [] identifier) =
    Name (getQualifiers (find identifier quals)) identifier
findName _ n = n

toLocals = toMap
toQualifieds modName ids = fmap (qualify modName) (toMap ids)
toMap ids = fromListUnique (fmap (\i -> (toId i, i)) ids)

-- Change qualified imports
-- e.g. import Viewer.Obj as Obj
-- Obj.load is changed to Viewer.Obj.Load
changeQualifiedImportsMod (ModuleDeclaration modName decls) =
    let quals = captureQualifiedImports decls
    in ModuleDeclaration modName (changeQualifiedImports quals decls)

changeQualifiedImports quals =
    let
        f (Variable n) =
            Variable (changeQualifier n quals)
        f (ConstructorExpression c) =
            ConstructorExpression (changeQualifier c quals)
        f (InfixOperator ea name eb) =
            InfixOperator ea (changeQualifier name quals) eb
        f e = e

        g (ConstructorPattern c ps) =
            ConstructorPattern (changeQualifier c quals) ps
        g (PatternInfixOperator p1 op p2) =
            PatternInfixOperator p1 (changeQualifier op quals) p2
        g p = p

        h (TypeInfixOperator ta op tb) =
            TypeInfixOperator ta (changeQualifier op quals) tb
        h (TypeConstructor c) =
            TypeConstructor (changeQualifier c quals)
        h t = t
    in transformBi f . transformBi g . transformBi h

captureQualifiedImports decls =
    [(nameToList renamedModName, nameToList modName) |
        ImportDeclaration modName Nothing (Just renamedModName) <- decls]

changeQualifier n@(Name modNames identifier) quals =
    case lookup modNames quals of
        Nothing -> n
        Just renamedModNames -> Name renamedModNames identifier
