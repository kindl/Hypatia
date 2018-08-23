{-# LANGUAGE OverloadedStrings #-}
module Compiler where


import Prelude hiding ((<>))
import Syntax
import Data.Text(Text, pack)
import Text.PrettyPrint(vcat, (<+>), (<>), ($$),
    equals, text, int, braces, parens, brackets, double, render, semi)


data Mod = Mod Name [Statement]

data Statement
    = Ret Expr
    | Assign Id Expr
    | If Expr [Statement] [Statement]
    | Imp Name
        deriving (Show)

data Expr
    = Var Name
    | LitD Double
    | LitT Text
    | Func [Id] [Statement]
    | Access Id [Int]
    | Call Expr [Expr]
    | Arr [Expr]
    | And Expr Expr
        deriving (Show)


-- Simplified language to Lua
renderLua sts = render (toLuaM sts)

toLuaM (Mod modName sts) =
    text "local" <+> flatModName modName <+> equals <+> text "{}"
        $$ vcatMap (toLuaT modName) sts
        $$ text "return" <+> flatModName modName

toLuaT modName (Assign x e) =
    flatVar (qualifyId modName x) <+> equals <+> toLuaE e
toLuaT _ s = toLuaS s

toLua sts = forwardLocals sts $$ vcatMap toLuaS sts

forwardLocals sts =
    vcat [text "local" <+> prettyId x | Assign x _ <- sts]

toLuaS (Assign x e) =
    prettyId x <+> equals <+> toLuaE e
toLuaS (Imp modName) =
    text "local" <+> flatModName modName <+> equals
        <+> text "require" <+> toLuaPath modName
toLuaS (Ret e) =
    text "return" <+> toLuaE e
toLuaS (If e th []) =
    text "if" <+> toLuaE e <+> text "then" $$ toLua th $$ text "end"
toLuaS (If e th el) =
    text "if" <+> toLuaE e <+> text "then"
        $$ toLua th $$ text "else"
        $$ toLua el $$ text "end"

toLuaE (Var x) = flatVar x
toLuaE (LitD d) = double d
toLuaE (LitT t) = text (show t)
toLuaE (Func vs sts) =
    parens (text "function" <> parens (commas (fmap prettyId vs))
        $$ toLua sts $$ text "end")
toLuaE (Access v indices) =
    prettyId v <> foldMap (brackets . int . (+ 1)) indices
toLuaE (Call e es) = toLuaE e <> parens (commas (fmap toLuaE es))
toLuaE (Arr es) = braces (commas (fmap toLuaE es))
toLuaE (And e1 e2) = toLuaE e1 <+> text "and" <+> toLuaE e2

{- JavaScript -}
renderJavaScript sts = render (toJavaScriptM sts)

toJavaScriptM (Mod modName sts) = text "const"
    <+> flatModName modName <+> equals <+> text "{}" <> semi
    $$ vcatMap (toJavaScriptT modName) sts
    $$ text "module.exports" <+> equals
    <+> flatModName modName <> semi

toJavaScriptT modName (Assign x e) = flatVar (qualifyId modName x)
    <+> equals <+> toJavaScriptE e <> semi
toJavaScriptT _ s = toJavaScriptS s

toJavaScriptS (Assign x e) =
    text "const" <+> prettyId x <+> equals <+> toJavaScriptE e <> semi
toJavaScriptS (Imp modName) =
    text "const" <+> flatModName modName <+> equals <+>
        text "require" <> parens (toJsPath modName) <> semi
toJavaScriptS (Ret e) =
    text "return" <+> toJavaScriptE e <> semi
toJavaScriptS (If e th []) =
    text "if" <> parens (toJavaScriptE e)
        $$ block (vcatMap toJavaScriptS th)
toJavaScriptS (If e th el) =
    text "if" <> parens (toJavaScriptE e)
        $$ block (vcatMap toJavaScriptS th)
        $$ text "else" $$ block (vcatMap toJavaScriptS el)

toJavaScriptE (Var x) = flatVar x
toJavaScriptE (LitD d) = double d
toJavaScriptE (LitT t) = text (show t)
toJavaScriptE (Func vs sts) =
    parens (text "function" <> parens (commas (fmap prettyId vs))
        $$ block (vcatMap toJavaScriptS sts))
toJavaScriptE (Access v indices) =
    prettyId v <> foldMap (brackets . int) indices
toJavaScriptE (Call e es) =
    toJavaScriptE e <> parens (commas (fmap toJavaScriptE es))
toJavaScriptE (Arr es) = brackets (commas (fmap toJavaScriptE es))
toJavaScriptE (And e1 e2) =
    toJavaScriptE e1 <+> text "&&" <+> toJavaScriptE e2

vcatMap f x = vcat (fmap f x)
block s = text "{" $$ s $$ text "}"

-- Compile to simplified language
compile (ModuleDeclaration modName decls) =
    Mod modName (foldMap (compileT modName) decls)

compileE (Variable v) = Var v
compileE (ConstructorExpression c) = Var c
compileE (FunctionApplication f e) = Call (compileE f) [compileE e]
compileE (LambdaExpression [VariablePattern v] e) =
    Func [v] (compileEtoS e)
compileE (LambdaExpression [p] e) =
    let
        v = makeId "_vl"
        err = Ret (mkError ("failed pattern match lambda at " ++ locationInfo p))
    in Func [v] (compileAlts v [err] [(p, e)])
compileE (ArrayExpression es) =
    Arr (fmap compileE es)
compileE (LiteralExpression l) =
    compileL l
compileE e@(CaseExpression _ _) =
    immediate (compileEtoS e)
compileE e@(LetExpression _ _) =
    immediate (compileEtoS e)
compileE e@(IfExpression _ _ _) =
    immediate (compileEtoS e)
compileE e = error ("compileE does not work on " ++ show e)

-- Compiles an expression in a statement context
-- TODO investigate
-- This seemed to be a good way of saving immediate functions
-- however nested case expressions lead to problems
-- e.g. multiple defined local _v
compileEtoS (CaseExpression e alts) =
    let
        v = makeId "_vc"
        err = Ret (mkError ("failed pattern match case at " ++ locationInfo (fmap fst alts)))
    in Assign v (compileE e) : compileAlts v [err] alts
compileEtoS (LetExpression decls e) =
    foldMap compileD decls ++ compileEtoS e
compileEtoS (IfExpression c th el) =
    [If (compileE c) (compileEtoS th) (compileEtoS el)]
compileEtoS e = [Ret (compileE e)]

compileL (Numeral n) = LitD n
compileL (Text t) = LitT t

compileT modName (TypeDeclaration _ _ cs) =
    fmap (compileConstructor modName) cs
compileT _ (ImportDeclaration modName _ _) = [Imp modName]
compileT _ (FixityDeclaration _ _ _ _) = []
compileT _ other = compileD other

-- TODO should patterns in expression declarations be allowed?
compileD (ExpressionDeclaration (VariablePattern x) e) =
    [Assign x (compileE e)]
compileD (ExpressionDeclaration Wildcard e) =
    [Assign (makeId "_vw") (compileE e)]
compileD (TypeSignature _ _) = []
compileD (AliasDeclaration _ _) = []
compileD other = error ("compileD does not work on " ++ show other)

compileConstructor _ (c, []) =
    Assign c (Func [] [])
compileConstructor modName (c, vs) =
    let
        name = qualifyId modName c
        vars = nNewVars (length vs)
        body = Arr (fmap Var (name:fmap fromId vars))
    in Assign c (curryFunc body vars)

curryFunc = foldr (\v r -> Func [v] [Ret r])

compileAlts v = foldr (\(p, e) rest ->
    let s = getAssignments v [] p ++ compileEtoS e
    in case getConditions v [] p of
        [] -> s
        cs -> If (foldr1 And cs) s []:rest)

getConditions v i (AliasPattern _ p) =
    getConditions v i p
getConditions v i (ConstructorPattern c []) =
    [mkEq (Access v i) (Var c)]
getConditions v i (ConstructorPattern c ps) =
    [mkIsArray (Access v i),
    mkEq (mkSize (Access v i)) (LitD (fromIntegral (length ps + 1))),
    mkEq (Access v (i ++ [0])) (Var c)]
    ++ descendAccess (getConditions v) i 1 ps
getConditions v i (ArrayPattern ps) =
    [mkIsArray (Access v i),
    mkEq (mkSize (Access v i)) (LitD (fromIntegral (length ps)))]
    ++ descendAccess (getConditions v) i 0 ps
getConditions v i (LiteralPattern l) =
    [mkEq (Access v i) (compileL l)]
getConditions _ _ Wildcard = []
getConditions _ _ (VariablePattern _) = []
getConditions _ _ p = error ("getConditions on " ++ pretty p)

getAssignments v i (VariablePattern x) =
    [Assign x (Access v i)]
getAssignments v i (AliasPattern x p) =
    Assign x (Access v i):getAssignments v i p
getAssignments v i (ConstructorPattern _ ps) =
    descendAccess (getAssignments v) i 1 ps
getAssignments v i (ArrayPattern ps) =
    descendAccess (getAssignments v) i 0 ps
getAssignments _ _ (LiteralPattern _) = []
getAssignments _ _ Wildcard = []
getAssignments _ _ p = error ("getAssignments on " ++ pretty p)

descendAccess _ _ _ [] = []
descendAccess f i j (p:ps) =
    f (i ++ [j]) p ++ descendAccess f i (j + 1) ps


immediate sts = Call (Func [] sts) []

mkVar = Var . fromText

mkError s = Call (mkVar "Native.error") [LitT (pack s)]
mkIsArray a = Call (mkVar "Native.isArray") [a]
mkEq a b = Call (Call (mkVar "Native.eq") [a]) [b]
mkSize a = Call (mkVar "Native.size") [a]

-- e.g. A module A.B is saved in the file A_B.lua
-- local A_B = require("A_B")
toLuaPath modName = text (show (renderFlatModName modName))

-- js needs the leading dot for local modules
toJsPath modName = text (show ("./" ++ renderFlatModName modName))
