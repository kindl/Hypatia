module Qualification where

import Syntax
import Data.Generics.Uniplate.Data(transformM, descendM, transformBi, transformBiM)
import Data.Foldable(foldMap')
import Control.Applicative(liftA2, liftA3)


-- Qualification is split into value level and type level,
-- otherwise type Unit = Unit would be ambiguous.

-- Aliases are mixed:
-- For example, in the declaration `infixl 7 * Tuple2`
-- Tuple2 might be a type or a value alias
qualifyAliases (ModuleDeclaration modName imports decls) =
    let
        h (AliasDeclaration v t) =
            AliasDeclaration (qualify modName v) t
        h (FixityDeclaration a p op alias) =
            FixityDeclaration a p (qualify modName op) (qualify modName alias)
        h decl = decl
    in ModuleDeclaration modName imports (fmap h decls)


-- Value Level
qualifyNames quals (ModuleDeclaration name imports decls) =
    fmap (ModuleDeclaration name imports) (traverse (qualifyD quals) decls)

-- Qualify the bindings and expressions appearing in declarations
qualifyD quals (ExpressionDeclaration p e) =
    liftA2 ExpressionDeclaration (qualifyP quals p) (qualifyE quals e)
qualifyD quals (TypeDeclaration v vars cs) =
    fmap (TypeDeclaration v vars) (traverse (firstA (findName quals)) cs)
qualifyD quals (TypeSignature v t) =
    fmap (flip TypeSignature t) (findName quals v)
qualifyD _ decl@(AliasDeclaration _ _) = Right decl
qualifyD _ decl@(FixityDeclaration _ _ _ _) = Right decl
qualifyD _ decl = Left ("Bug: Unexpected declaration " ++ show decl)


qualifyP quals pat =
    let
        f (VariablePattern v) =
            fmap VariablePattern (findName quals v)
        f (AliasPattern v p) =
            fmap (flip AliasPattern p) (findName quals v)
        f (ConstructorPattern c ps) =
            fmap (flip ConstructorPattern ps) (findName quals c)
        f (PatternInfixOperator p1 op p2) =
            fmap (\q -> PatternInfixOperator p1 q p2) (findName quals op)
        f p = Right p
    in transformM f pat


qualifyE quals (Variable n) =
    fmap Variable (findName quals n)
qualifyE quals (ConstructorExpression c) =
    fmap ConstructorExpression (findName quals c)
qualifyE quals (LetExpression decls e) =
    let
        locals = toLocals (foldMap' captureNameD decls)
        newQuals = unionUnique locals quals
    in liftA2 LetExpression (traverse (qualifyD newQuals) decls)
        (qualifyE newQuals e)
qualifyE quals (CaseExpression e alts) =
    liftA2 CaseExpression (qualifyE quals e) (traverse (qualifyAlt quals) alts)
qualifyE quals (LambdaExpression [p] e) = do
    (qp, qe) <- qualifyAlt quals (p, e)
    pure (LambdaExpression [qp] qe)
qualifyE quals (InfixOperator ea name eb) =
    liftA3 InfixOperator (qualifyE quals ea) (findName quals name) (qualifyE quals eb)
qualifyE quals e =
    descendM (qualifyE quals) e


qualifyAlt quals (p, e) =
    let
        locals = toLocals (getDefsP p)
        newQuals = unionUnique locals quals
    in liftA2 (,) (qualifyP newQuals p) (qualifyE newQuals e)


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
        h (TypeInfixOperator ta op tb) =
            fmap (\q -> TypeInfixOperator ta q tb) (findName quals op)
        h (TypeConstructor c) =
            fmap TypeConstructor (findName quals c)
        h t = Right t
    in transformBiM h =<< fmap (ModuleDeclaration name) (traverse (qualifyTypesD quals) decls)

-- The constructors itself are on value level
qualifyTypesD quals (TypeDeclaration v vs cs) =
    fmap (\q -> TypeDeclaration q vs cs) (findName quals v)
qualifyTypesD _ decl = Right decl

-- The constructors itself are on value level
captureTypeNames (ModuleDeclaration modName _ decls) =
    toQualifieds modName (foldMap' captureTypeNamesTopDecl decls)

captureTypeNamesTopDecl (AliasDeclaration identifier _) = [identifier]
captureTypeNamesTopDecl (TypeDeclaration identifier _ _) = [identifier]
captureTypeNamesTopDecl _ = []

findName quals (Name [] identifier) = do
    q <- findEither identifier quals
    pure (Name (getQualifiers q) identifier)
findName _ n = Right n

toLocals = toMap
toQualifieds modName ids = fmap (qualify modName) (toMap ids)
toMap ids = fromListUnique (fmap (\i -> (toId i, i)) ids)

-- Change qualified imports
-- e.g. import Viewer.Obj as Obj
-- Obj.load is changed to Viewer.Obj.Load
changeQualifiedImportsMod (ModuleDeclaration modName imports decls) =
    let quals = captureQualifiedImports decls
    in ModuleDeclaration modName imports (changeQualifiedImports quals decls)

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

captureQualifiedImports imports =
    [(nameToList renamedModName, nameToList modName) |
        ImportDeclaration modName Nothing (Just renamedModName) <- imports]

changeQualifier n@(Name modNames identifier) quals =
    case lookup modNames quals of
        Nothing -> n
        Just renamedModNames -> Name renamedModNames identifier
