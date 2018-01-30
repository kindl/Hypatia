module Qualification where

import Syntax
import Data.HashMap.Strict(fromList)
import Data.Generics.Uniplate.Data(transform, descend)


qualifyNames quals (ModuleDeclaration name decls) =
    ModuleDeclaration name (fmap (qualifyD quals) decls)

qualifyD quals (EnumDeclaration id vars constructors) =
    EnumDeclaration id vars (fmap (fmap (fmap (qualifyT quals))) constructors)
qualifyD quals (ExpressionDeclaration p e) =
    ExpressionDeclaration (qualifyP quals p) (qualifyE quals e)
qualifyD quals (AliasDeclaration id t) =
    AliasDeclaration id (qualifyT quals t)
qualifyD quals (TypeSignature id t) =
    TypeSignature id (qualifyT quals t)
qualifyD quals (FunctionDeclaration id ps e) =
    FunctionDeclaration id (fmap (qualifyP quals) ps) (qualifyE quals e)
qualifyD _ decl@(ImportDeclaration _ _ _) = decl
qualifyD _ decl@(FixityDeclaration _ _ _ _) = decl

qualifyT quals ty =
    let
        f (TypeInfixOperator ta op tb) =
            TypeInfixOperator ta (findName op quals) tb
        f (TypeConstructor c) =
            TypeConstructor (findName c quals)
        f t = t
    in transform f ty

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
    let newQuals = toLocals (foldMap captureNameD decls) `mappend` quals
    in LetExpression (fmap (qualifyD newQuals) decls) (qualifyE newQuals e)
qualifyE quals (CaseLambdaExpression alts) =
    CaseLambdaExpression (fmap (qualifyA quals) alts)
qualifyE quals (LambdaExpression [p] e) =
    let (qp, qe) = qualifyA quals (p, e)
    in LambdaExpression [qp] qe
qualifyE quals (InfixOperator ea name eb) =
    InfixOperator (qualifyE quals ea) (findName name quals) (qualifyE quals eb)
qualifyE quals e =
    descend (qualifyE quals) e

qualifyA quals (p, e) =
    let newQuals = toLocals (getDefsP p) `mappend` quals
    in (qualifyP newQuals p, qualifyE newQuals e)


captureNames (ModuleDeclaration modName decls) =
    toQualifieds modName (foldMap captureNameD decls)

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
    Name (getQualifiers (findId id quals)) id
findName n _ = n

toLocals ids = fmap fromId (toMap ids)
toQualifieds modName ids = fmap (qualifyId modName) (toMap ids)
toMap ids = fromList (fmap (\id -> (id, id)) ids)
