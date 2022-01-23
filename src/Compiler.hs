{-# LANGUAGE OverloadedStrings #-}
module Compiler where


import Prelude hiding ((<>))
import Syntax
import Data.List(nub)
import Data.Text(Text, pack)
import Text.PrettyPrint(vcat, nest, render, (<+>), (<>),
    equals, text, int, braces, parens, brackets, double, semi)


data Mod = Mod Name [Statement]

data Statement
    = Ret Expr
    | Assign Id Expr
    | If Expr [Statement] [Statement]
    | Imp Name
        deriving (Show)

data Expr
    = Var Name
    | LitI Int
    | LitD Double
    | LitT Text
    | Func [Id] [Statement]
    | Access Id [Int]
    | Call Expr [Expr]
    | Arr [Expr]
    | And Expr Expr
    | Eq Expr Expr
        deriving (Show)


-- Render simplified language as Lua
renderLua sts = render (toLuaM sts)

toLuaM (Mod modName sts) = vcat [
    text "local" <+> flatModName modName <+> equals <+> text "{}",
    vcatMap (toLuaT modName) sts,
    text "return" <+> flatModName modName]

toLuaT modName (Assign x (Func vs sts)) =
    toLuaFun (flatVar (qualifyId modName x) <+> equals) vs sts
toLuaT modName (Assign x e) =
    flatVar (qualifyId modName x) <+> equals <+> toLuaE e
toLuaT _ s = toLuaS s

-- local functions need to be declared beforehand for recursion
--local fix = function(f) return f(fix(f)) end
-- would result in an error because "fix" is undefined
toLuaS (Assign x (Func vs sts)) = vcat [
    text "local" <+> prettyId x,
    toLuaFun (prettyId x <+> equals) vs sts]
toLuaS (Assign x e) =
    text "local" <+> prettyId x <+> equals <+> toLuaE e
toLuaS (Imp modName) =
    text "local" <+> flatModName modName <+> equals
        <+> text "require" <+> toLuaPath modName
toLuaS (Ret (Func vs sts)) =
    toLuaFun (text "return") vs sts
toLuaS (Ret e) =
    text "return" <+> toLuaE e
toLuaS (If e th []) = vcat [
    text "if" <+> toLuaE e <+> text "then",
    nest 4 (vcatMap toLuaS th),
    text "end"]
toLuaS (If e [] th) = vcat [
    text "if not" <+> parens (toLuaE e) <+> text "then",
    nest 4 (vcatMap toLuaS th),
    text "end"]
toLuaS (If e th el) = vcat [
    text "if" <+> toLuaE e <+> text "then",
    nest 4 (vcatMap toLuaS th),
    text "else",
    nest 4 (vcatMap toLuaS el),
    text "end"]

toLuaE (Var x) = flatVar x
toLuaE (LitI i) = int i
toLuaE (LitD d) = double d
toLuaE (LitT t) = text (show t)
toLuaE (Func vs sts) =
    toLuaFun mempty vs sts
toLuaE (Access v indices) =
    prettyId v <> foldMap (brackets . int . (+ 1)) indices
-- Add parantheses for immediate functions
-- (function () print "Hi" end)() would be a syntax error
-- without parentheses 
toLuaE (Call e@(Func _ _) es) =
    parens (toLuaE e) <> parens (commas (fmap toLuaE es))
toLuaE (Call e es) = toLuaE e <> parens (commas (fmap toLuaE es))
toLuaE (Arr es) = braces (commas (fmap toLuaE es))
toLuaE (And e1 e2) = toLuaE e1 <+> text "and" <+> toLuaE e2
toLuaE (Eq e1 e2) = toLuaE e1 <+> text "==" <+> toLuaE e2

toLuaFun prefix vs sts = vcat [
    prefix <+> text "function" <> parens (commas (fmap prettyId vs)),
    nest 4 (vcatMap toLuaS sts),
    text "end"]

-- e.g. A module A.B is saved in the file A_B.lua
-- local A_B = require("A_B")
toLuaPath modName = text (show (renderFlatModName modName))


-- Render simplified language as JavaScript
renderJavaScript sts = render (toJavaScriptM sts)

toJavaScriptM (Mod modName sts) = vcat [
    text "const" <+> flatModName modName <+> equals <+> text "{}" <> semi,
    vcatMap (toJavaScriptT modName) sts,
    text "module.exports" <+> equals <+> flatModName modName <> semi]

toJavaScriptT modName (Assign x (Func vs sts)) =
    toJavaScriptFun (flatVar (qualifyId modName x) <+> equals) vs sts <> semi
toJavaScriptT modName (Assign x e) =
    flatVar (qualifyId modName x) <+> equals <+> toJavaScriptE e <> semi
toJavaScriptT _ s = toJavaScriptS s

toJavaScriptS (Assign x e) =
    text "const" <+> prettyId x <+> equals <+> toJavaScriptE e <> semi
toJavaScriptS (Imp modName) =
    text "const" <+> flatModName modName <+> equals <+>
        text "require" <> parens (toJsPath modName) <> semi
toJavaScriptS (Ret (Func vs sts)) =
    toJavaScriptFun (text "return") vs sts <> semi
toJavaScriptS (Ret e) =
    text "return" <+> toJavaScriptE e <> semi
toJavaScriptS (If e th []) = vcat [
    text "if" <> parens (toJavaScriptE e) <+> text "{",
    nest 4 (vcatMap toJavaScriptS th),
    text "}"]
toJavaScriptS (If e [] th) = vcat [
    text "if" <> parens (text "!" <> parens (toJavaScriptE e)) <+> text "{",
    nest 4 (vcatMap toJavaScriptS th),
    text "}"]
toJavaScriptS (If e th el) = vcat [
    text "if" <> parens (toJavaScriptE e) <+> text "{",
    nest 4 (vcatMap toJavaScriptS th),
    text "}" <+> text "else" <+> text "{",
    nest 4 (vcatMap toJavaScriptS el),
    text "}"]

toJavaScriptE (Var x) = flatVar x
toJavaScriptE (LitI i) = int i
toJavaScriptE (LitD d) = double d
toJavaScriptE (LitT t) = text (show t)
toJavaScriptE (Func vs sts) =
    toJavaScriptFun mempty vs sts
toJavaScriptE (Access v indices) =
    prettyId v <> foldMap (brackets . int) indices
-- Add parantheses for immediate functions
toJavaScriptE (Call e@(Func _ _) es) =
    parens (toJavaScriptE e) <> parens (commas (fmap toJavaScriptE es))
toJavaScriptE (Call e es) =
    toJavaScriptE e <> parens (commas (fmap toJavaScriptE es))
toJavaScriptE (Arr es) = brackets (commas (fmap toJavaScriptE es))
toJavaScriptE (And e1 e2) =
    toJavaScriptE e1 <+> text "&&" <+> toJavaScriptE e2
toJavaScriptE (Eq e1 e2) =
    toJavaScriptE e1 <+> text "===" <+> toJavaScriptE e2

toJavaScriptFun prefix vs sts = vcat [
    prefix <+> text "function" <> parens (commas (fmap prettyId vs)) <+> text "{",
    nest 4 (vcatMap toJavaScriptS sts),
    text "}"]

-- js needs the leading dot for local modules
toJsPath modName = text (show ("./" ++ renderFlatModName modName))


-- Compile to simplified language
compile (ModuleDeclaration modName decls) =
    let
        compiledDecls = foldMap (compileTop modName) decls
        imports = compileImports decls
    in Mod modName (imports ++ compiledDecls)

compileE (Variable v) = Var v
compileE (ConstructorExpression c) = Var c
compileE (FunctionApplication f e) = Call (compileE f) [compileE e]
compileE (LambdaExpression [VariablePattern v] e) =
    Func [v] (compileEtoS e)
compileE (LambdaExpression [p] e) =
    let
        v = prefixedId "l"
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

{-
Compiles an expression in a statement context
TODO investigate
Compiling an expressions to a statement seemed to be a good way of saving immediate functions
however nested case expressions lead to problems e.g. multiple defined local _v
-}
compileEtoS (CaseExpression e alts) =
    let
        v = prefixedId "c"
        err = Ret (mkError ("failed pattern match case at " ++ locationInfo (fmap fst alts)))
    in Assign v (compileE e) : compileAlts v [err] alts
compileEtoS (LetExpression decls e) =
    foldMap compileD decls ++ compileEtoS e
compileEtoS (IfExpression c th el) =
    [If (compileE c) (compileEtoS th) (compileEtoS el)]
compileEtoS e = [Ret (compileE e)]

compileL (Numeral n) = LitD n
compileL (Text t) = LitT t

-- Compile top level declarations
compileTop modName (TypeDeclaration _ _ cs) =
    fmap (compileConstructor modName) cs
compileTop _ (ImportDeclaration _ _ _) = []
compileTop _ (FixityDeclaration _ _ _ _) = []
compileTop _ other = compileD other

{-
Only import a module once in cases as for example
import Viewer.Obj(getFaces)
import Viewer.Obj as Obj
-}
compileImports decls = fmap Imp (nub [modName |
    ImportDeclaration modName _ _ <- decls])

compileD (ExpressionDeclaration (VariablePattern x) e) =
    [Assign x (compileE e)]
compileD (ExpressionDeclaration (Wildcard _) e) =
    [Assign (prefixedId "w") (compileE e)]
{-
Compile pattern matches in let expressions like
let
    Tuple a b = Tuple 2 3
in write (toString a)

To work on the top level, v would need to have the module name included
-}
compileD (ExpressionDeclaration p pe) =
    let
        v = prefixedId "d"
        err = Ret (mkError ("failed pattern match declaration at " ++ locationInfo p))
        s = getAssignments v [] p
        cs = getConditions v [] p
    in Assign v (compileE pe) : If (foldr1 And cs) [] [err] : s
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


{-
Compiling case expressions

A case expression of the form
    case e of {p1 -> e1; p2 -> e2; ...}
is converted to if-statements as follows
    local v = e
    if matches(p1, v) then
        local a1, a2, ... = v[1], v[2][3], ...
        return e1
    end
    if matches(p2, v) then
        local a1, a2, ... = v[1], v[2][3], ...
        return e2
    end
    return error
v is a temporal variable and contains the value of expression e
a are variables appearing in the pattern p

A constructor returns an array, for example in
type Option a = Just a | Nothing
Just is compiled as
    local Just = function(x)
        return {Just, x}
    end
Matching on the pattern (Just y) compiles to
    if isArray(v) and #v == 2 and v[1] == Just then
        local y = v[2]
        ...
    end

descendAccess is used to recurse deeper into the patterns.
The y in (Just y) could be any pattern and not only a variable.
-}
compileAlts v = foldr (\(p, e) rest ->
    let s = getAssignments v [] p ++ compileEtoS e
    in case getConditions v [] p of
        [] -> s
        cs -> If (foldr1 And cs) s []:rest)

getConditions v i (AliasPattern _ p) =
    getConditions v i p
getConditions v i (ConstructorPattern c []) =
    [Eq (Access v i) (Var c)]
getConditions v i (ConstructorPattern c ps) =
    [mkIsArray (Access v i),
    Eq (mkSize (Access v i)) (LitI (length ps + 1)),
    Eq (Access v (i ++ [0])) (Var c)]
    ++ descendAccess (getConditions v) i 1 ps
getConditions v i (ArrayPattern ps) =
    [mkIsArray (Access v i),
    Eq (mkSize (Access v i)) (LitI (length ps))]
    ++ descendAccess (getConditions v) i 0 ps
getConditions v i (LiteralPattern l) =
    [Eq (Access v i) (compileL l)]
getConditions _ _ (Wildcard _) = []
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
getAssignments _ _ (Wildcard _) = []
getAssignments _ _ p = error ("getAssignments on " ++ pretty p)

descendAccess _ _ _ [] = []
descendAccess f i j (p:ps) =
    f (i ++ [j]) p ++ descendAccess f i (j + 1) ps


immediate sts = Call (Func [] sts) []


mkError s = Call (Var (fromText "Native.error")) [LitT (pack s)]
mkIsArray a = Call (Var (fromText "Native.isArray")) [a]
mkSize a = Call (Var (fromText "Native.size")) [a]


vcatMap f x = vcat (fmap f x)
