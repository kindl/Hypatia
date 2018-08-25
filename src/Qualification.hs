module Qualification where

import Syntax
import Data.Generics.Uniplate.Data(transform, descend, transformBi)


{-
Values
split into value level and type level
otherwise type Unit = Unit is problematic
-}
qualifyNames quals (ModuleDeclaration name decls) =
    ModuleDeclaration name (fmap (qualifyD quals) decls)

qualifyD quals (ExpressionDeclaration p e) =
    ExpressionDeclaration (qualifyP quals p) (qualifyE quals e)
qualifyD _ decl@(TypeDeclaration _ _ _) = decl
qualifyD _ decl@(TypeSignature _ _) = decl
qualifyD _ decl@(ImportDeclaration _ _ _) = decl
qualifyD _ decl@(FixityDeclaration _ _ _ _) = decl
qualifyD _ decl@(AliasDeclaration _ _) = decl
qualifyD _ decl@(FunctionDeclaration _ _ _) = decl


qualifyP quals pat =
    let
        f (ConstructorPattern c ps) =
            ConstructorPattern (findName c quals) ps
        f (PatternInfixOperator p1 op p2) =
            PatternInfixOperator p1 (findName op quals) p2
        f p = p
    in transform f pat


qualifyE quals (Variable n) =
    Variable (findName n quals)
qualifyE quals (ConstructorExpression c) =
    ConstructorExpression (findName c quals)
qualifyE quals (LetExpression decls e) =
    let
        locals = toLocals (foldMap captureNameD decls)
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
        (findName name quals) (qualifyE quals eb)
qualifyE quals e =
    descend (qualifyE quals) e

qualifyA quals (p, e) =
    let
        locals = toLocals (getDefsP p)
        newQuals = unionUnique locals quals
    in (qualifyP newQuals p, qualifyE newQuals e)


captureNames (ModuleDeclaration modName decls) = mappend
    (toQualifieds modName (foldMap captureNameD decls))
    (toQualifieds modName (foldMap captureTopName decls))

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

{- Types -}
qualifyTypeNames quals m =
    let
        f (TypeInfixOperator ta op tb) =
            TypeInfixOperator ta (findName op quals) tb
        f (TypeConstructor c) =
            TypeConstructor (findName c quals)
        f t = t
    in transformBi f m

captureTypeNames (ModuleDeclaration modName decls) =
    toQualifieds modName (foldMap captureTypeNameD decls)

captureTypeNameD (AliasDeclaration identifier _) = [identifier]
captureTypeNameD (TypeDeclaration identifier _ _) = [identifier]
captureTypeNameD _ = []


findName (Name [] identifier) quals =
    Name (getQualifiers (find identifier quals)) identifier
findName n _ = n

toLocals ids = fmap fromId (toMap ids)
toQualifieds modName ids = fmap (qualifyId modName) (toMap ids)
toMap ids = fromListUnique (fmap (\i -> (i, i)) ids)

{- Resolve qualified imports -}
resolveQualifiedImports (ModuleDeclaration modName decls) =
    let quals = captureQualifiedImportDeclarations decls
    in ModuleDeclaration modName (renameQualifiedNames quals decls)

renameQualifiedNames quals =
    let
        f (Variable n) =
            Variable (renameQualified n quals)
        f (ConstructorExpression c) =
            ConstructorExpression (renameQualified c quals)
        f (InfixOperator ea name eb) =
            InfixOperator ea (renameQualified name quals) eb
        f e = e

        g (ConstructorPattern c ps) =
            ConstructorPattern (renameQualified c quals) ps
        g (PatternInfixOperator p1 op p2) =
            PatternInfixOperator p1 (renameQualified op quals) p2
        g p = p

        h (TypeInfixOperator ta op tb) =
            TypeInfixOperator ta (renameQualified op quals) tb
        h (TypeConstructor c) =
            TypeConstructor (renameQualified c quals)
        h t = t
    in transformBi f . transformBi g . transformBi h

captureQualifiedImportDeclarations decls =
    [(nameToList renamedModName, nameToList modName) |
        ImportDeclaration modName Nothing (Just renamedModName) <- decls]

renameQualified n@(Name modNames identifier) quals =
    case lookup modNames quals of
        Nothing -> n
        Just renamedModNames -> Name renamedModNames identifier
