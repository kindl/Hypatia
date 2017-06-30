module Qualification where

import Syntax
import Data.HashMap.Strict(fromList)


qualifyM (ModuleDeclaration modName decls) =
    let
        imps = fromList (foldMap captureImport decls)
        quals = toQualifieds modName (foldMap captureNameD decls) `mappend` imps
    in ModuleDeclaration modName (fmap (qualifyD quals) decls)

qualifyD quals (EnumDeclaration id vars constructors) =
    EnumDeclaration id vars (fmap (qualifyC quals) constructors)
qualifyD quals (ExpressionDeclaration p e) =
    ExpressionDeclaration (qualifyP quals p) (qualifyE quals e)
qualifyD quals (AliasDeclaration id t) =
    AliasDeclaration id (qualifyT quals t)
qualifyD quals (TypeSignature id t) =
    TypeSignature id (qualifyT quals t)
qualifyD quals (FunctionDeclaration id ps e) =
    FunctionDeclaration id (fmap (qualifyP quals) ps) (qualifyE quals e)
qualifyD _ (ImportDeclaration modName spec) =
    ImportDeclaration modName spec
qualifyD _ (FixityDeclaration a p id name) =
    FixityDeclaration a p id name

qualifyC quals (c, ts) =
    (c, fmap (qualifyT quals) ts)

qualifyT quals (TypeArrow ta tb) =
    TypeArrow (qualifyT quals ta) (qualifyT quals tb)
qualifyT quals (TypeInfixOperator ta op tb) =
    TypeInfixOperator (qualifyT quals ta) (findName op quals) (qualifyT quals tb)
qualifyT quals (TypeApplication ta tb) =
    TypeApplication (qualifyT quals ta) (qualifyT quals tb)
qualifyT quals (TypeConstructor c) =
    TypeConstructor (findName c quals)
qualifyT quals (ParenthesizedType t) =
    ParenthesizedType (qualifyT quals t)
qualifyT quals (ForAll vs t) =
    ForAll vs (qualifyT quals t)
qualifyT _ (TypeVariable v) =
    TypeVariable v
qualifyT _ (SkolemConstant c) =
    SkolemConstant c
qualifyT _ (UniVariable u) =
    UniVariable u

qualifyP quals (ConstructorPattern c ps) =
    ConstructorPattern (findName c quals) (fmap (qualifyP quals) ps)
qualifyP quals (ArrayPattern ps) =
    ArrayPattern (fmap (qualifyP quals) ps)
qualifyP quals (AliasPattern id p) =
    AliasPattern id (qualifyP quals p)
qualifyP quals (ParenthesizedPattern p) =
    ParenthesizedPattern (qualifyP quals p)
qualifyP quals (InfixConstructorPattern p1 op p2) =
    InfixConstructorPattern (qualifyP quals p1) (findName op quals) (qualifyP quals p2)
qualifyP _ (VariablePattern v) =
    VariablePattern v
qualifyP _ (LiteralPattern l) =
    LiteralPattern l
qualifyP _ Wildcard =
    Wildcard

qualifyE quals (Variable n) =
    Variable (findName n quals)
qualifyE quals (ConstructorExpression c) =
    ConstructorExpression (findName c quals)
qualifyE quals (FunctionApplication ea eb) =
    FunctionApplication (qualifyE quals ea) (qualifyE quals eb)
qualifyE quals (LetExpression decls e) =
    let newQuals = toLocals (foldMap captureNameD decls) `mappend` quals
    in  LetExpression (fmap (qualifyD newQuals) decls) (qualifyE newQuals e)
qualifyE quals (CaseLambdaExpression alts) =
    CaseLambdaExpression (fmap (qualifyA quals) alts)
qualifyE quals (ParenthesizedExpression e) =
    ParenthesizedExpression (qualifyE quals e)
qualifyE quals (ArrayExpression es) =
    ArrayExpression (fmap (qualifyE quals) es)
qualifyE quals (LambdaExpression [p] e) =
    let (qp, qe) = qualifyA quals (p, e)
    in LambdaExpression [qp] qe
qualifyE quals (InfixOperator ea name eb) =
    InfixOperator (qualifyE quals ea) (findName name quals) (qualifyE quals eb)
qualifyE quals (PrefixNegation e) =
    PrefixNegation (qualifyE quals e)
qualifyE quals (IfExpression c th el) =
    IfExpression (qualifyE quals c) (qualifyE quals th) (qualifyE quals el)
--qualifyE quals (TypeAnnotation e t) =
--    TypeAnnotation (qualifyE quals e) (qualifyT quals t)
qualifyE _ e = e

qualifyA quals (p, e) =
    let newQuals = toLocals (getDefsP p) `mappend` quals
    in (qualifyP newQuals p, qualifyE newQuals e)

captureImport (ImportDeclaration modName (Just ids)) =
    fmap (\id -> (id, qualifyId modName id)) ids
captureImport _ = []

captureNameD (EnumDeclaration t _ cs) =
    t : fmap fst cs
captureNameD (ExpressionDeclaration p _) =
    getDefsP p
captureNameD (AliasDeclaration id _) =
    [id]
captureNameD (TypeSignature id _) =
    [id]
captureNameD (FixityDeclaration _ _ id _) =
    [id]
captureNameD _ = []

findName (Name [] id) quals =
    find id quals
findName n _ = n

toLocals ids = fromList (fmap (\id -> (id, fromId id)) ids)
toQualifieds modName ids = fromList (fmap (\id -> (id, qualifyId modName id)) ids)
