module Qualification where

import Syntax
import Prelude hiding (lookup)
import Data.HashMap.Strict(lookup, mapKeys, fromList, lookupDefault)
import Data.Generics.Uniplate.Data(transformM, descendM, transformBi, transformBiM)
import Data.Foldable(foldMap')
import Control.Applicative(liftA3)
import Control.Monad((>=>))

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
qualifyD quals (FunctionDeclaration v alts) =
    liftA2 FunctionDeclaration (findName quals v) (traverse (qualifyMultiAlt quals) alts)
qualifyD quals (TypeDeclaration v vars cs) =
    fmap (TypeDeclaration v vars) (traverse (firstA (findName quals)) cs)
qualifyD quals (TypeSignature v t) =
    fmap (flip TypeSignature t) (findName quals v)
qualifyD _ decl@(AliasDeclaration _ _) = Right decl
qualifyD _ decl@(FixityDeclaration _ _ _ _) = Right decl

qualifyP quals pat =
    let
        f (VariablePattern v) =
            fmap VariablePattern (findName quals v)
        f (AliasPattern v p) =
            fmap (flip AliasPattern p) (findName quals v)
        f (ConstructorPattern info c ps) =
            fmap (\q -> ConstructorPattern info q ps) (findName quals c)
        f (OperatorPattern p1 op p2) =
            fmap (\q -> OperatorPattern p1 q p2) (findName quals op)
        f p = Right p
    in transformM f pat


qualifyE quals (Variable n) =
    fmap Variable (findName quals n)
qualifyE quals (ConstructorExpression c) =
    fmap ConstructorExpression (findName quals c)
qualifyE quals (LetExpression decls e) = do
    locals <- toLocals (foldMap' captureDecl decls)
    newQuals <- unionUnique locals quals
    liftA2 LetExpression (traverse (qualifyD newQuals) decls)
        (qualifyE newQuals e)
qualifyE quals (CaseExpression e alts) =
    liftA2 CaseExpression (qualifyE quals e) (traverse (qualifyAlt quals) alts)
qualifyE quals (LambdaExpression [p] e) = do
    (qp, qe) <- qualifyAlt quals (p, e)
    pure (LambdaExpression [qp] qe)
qualifyE quals (OperatorExpression ea name eb) =
    liftA3 OperatorExpression (qualifyE quals ea) (findName quals name) (qualifyE quals eb)
qualifyE quals e =
    descendM (qualifyE quals) e

qualifyMultiAlt quals (ps, e) = do
    locals <- toLocals (foldMap' getBindings ps)
    newQuals <- unionUnique locals quals
    liftA2 (,) (traverse (qualifyP newQuals) ps) (qualifyE newQuals e)

qualifyAlt quals (p, e) = do
    locals <- toLocals (getBindings p)
    newQuals <- unionUnique locals quals
    liftA2 (,) (qualifyP newQuals p) (qualifyE newQuals e)

captureNames (ModuleDeclaration modName _ decls) = do
    -- Here toQualifieds is used seperately on purpose
    -- for example, combining the name from a declaration
    -- and a signature should not result in a shadowing error.
    fromSigs <- toQualifieds modName (foldMap' captureSignature decls)
    fromTopDecls <- toQualifieds modName (foldMap' captureTopDecl decls)
    fromDecls <- toQualifieds modName (foldMap' captureDecl decls)
    return (fromSigs <> fromTopDecls <> fromDecls)

captureDecl (ExpressionDeclaration p _) = getBindings p
captureDecl (FunctionDeclaration v _) = [v]
captureDecl _ = []

captureSignature (TypeSignature identifier _) = [identifier]
captureSignature _ = []

captureTopDecl (TypeDeclaration _ _ cs) = fmap fst cs
captureTopDecl (AliasDeclaration identifier _) = [identifier]
captureTopDecl (FixityDeclaration _ _ op _) = [op]
captureTopDecl _ = []


-- Type Level
qualifyTypeNames quals m =
    let
        h (TypeOperator ta op tb) =
            fmap (\q -> TypeOperator ta q tb) (findName quals op)
        h (TypeConstructor c) =
            fmap TypeConstructor (findName quals c)
        h t = Right t

        k (TypeDeclaration v vs cs) =
            fmap (\q -> TypeDeclaration q vs cs) (findName quals v)
        k decl = Right decl
    in (transformBiM h >=> transformBiM k) m

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

toLocals ids = fmap (mapKeys toId) (fromListUnique ids)
toQualifieds modName ids = fmap (fmap (qualify modName)) (toLocals ids)

-- Change qualified imports
-- e.g. import Viewer.Obj as Obj
-- Obj.load is changed to Viewer.Obj.Load
changeQualifiedImportsMod (ModuleDeclaration modName imports decls) =
    let quals = fromList (captureQualifiedImports imports)
    in ModuleDeclaration modName imports (changeQualifiedImports quals decls)

changeQualifiedImports quals =
    let
        f (Variable n) =
            Variable (changeQualifier n quals)
        f (ConstructorExpression c) =
            ConstructorExpression (changeQualifier c quals)
        f (OperatorExpression ea name eb) =
            OperatorExpression ea (changeQualifier name quals) eb
        f e = e

        g (ConstructorPattern info c ps) =
            ConstructorPattern info (changeQualifier c quals) ps
        g (OperatorPattern p1 op p2) =
            OperatorPattern p1 (changeQualifier op quals) p2
        g p = p

        h (TypeOperator ta op tb) =
            TypeOperator ta (changeQualifier op quals) tb
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

-- Tag info annotation for optimiziation
annotateTagInfo tagInfos =
    let
        g (ConstructorPattern _ c ps) =
            ConstructorPattern (lookupDefault TaggedRepresentation c tagInfos) c ps
        g p = p
    in transformBi g

captureTagInfo (ModuleDeclaration _ _ decls) =
    fromList [(c, ArrayRepresentation) | TypeDeclaration _ _ [(c, _)] <- decls]
