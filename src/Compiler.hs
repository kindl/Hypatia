{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import Syntax
import Data.List(nub)
import Data.Text(Text)
import Prettyprinter(vcat, indent, (<+>),
    equals, braces, parens, brackets, semi, pretty, dquotes)
import Data.Foldable(foldMap')


type Import = Name

data Mod = Mod Name [Import] [Statement]

data Statement
    = Ret Expr
    | Assign Binding Expr
    | If Expr [Statement] [Statement]
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

toLuaM (Mod modName imports statements) = vcat [
    vcatMap toLuaI imports,
    text "local" <+> flatModName modName <+> equals <+> text "{}",
    vcatMap toLuaS statements,
    text "return" <+> flatModName modName]

toLuaI modName =
    text "local" <+> flatModName modName <+> equals
        <+> text "require" <+> toLuaPath modName

-- local functions need to be declared beforehand for recursion
-- `local fix = function(f) return f(fix(f)) end`
-- would result in an error because `fix` is undefined
toLuaS (Assign (Name [] x) e@(Func _ _)) = vcat [
    text "local" <+> pretty x,
    pretty x <+> equals <+> toLuaE e]
toLuaS (Assign (Name [] x) e) =
    text "local" <+> pretty x <+> equals <+> toLuaE e
-- Qualified names do not need "local"
toLuaS (Assign x e) =
    flatName x <+> equals <+> toLuaE e
toLuaS (Ret e) =
    text "return" <+> toLuaE e
toLuaS (If e th []) = vcat [
    text "if" <+> toLuaE e <+> text "then",
    indent 4 (vcatMap toLuaS th),
    text "end"]
toLuaS (If e [] th) = vcat [
    text "if not" <+> parens (toLuaE e) <+> text "then",
    indent 4 (vcatMap toLuaS th),
    text "end"]
toLuaS (If e th el) = vcat [
    text "if" <+> toLuaE e <+> text "then",
    indent 4 (vcatMap toLuaS th),
    text "else",
    indent 4 (vcatMap toLuaS el),
    text "end"]

toLuaE (Var x) = flatName x
toLuaE (LitI i) = pretty i
toLuaE (LitD d) = prettyNumeral d
toLuaE (LitT t) = prettyEscaped t
toLuaE (Func vs sts) = vcat [
    text "function" <> parens (commas (fmap pretty vs)),
    indent 4 (vcatMap toLuaS sts),
    text "end"]
toLuaE (Access v indices) =
    pretty v <> foldMap' (brackets . pretty . (+ 1)) indices
-- Add parantheses for immediate functions
-- (function () print "Hi" end)() would be a syntax error
-- without parentheses 
toLuaE (Call e@(Func _ _) es) =
    parens (toLuaE e) <> parens (commas (fmap toLuaE es))
toLuaE (Call e es) = toLuaE e <> parens (commas (fmap toLuaE es))
toLuaE (Arr es) = braces (commas (fmap toLuaE es))
toLuaE (And e1 e2) = toLuaE e1 <+> text "and" <+> toLuaE e2
toLuaE (Eq e1 e2) = toLuaE e1 <+> text "==" <+> toLuaE e2

-- e.g. A module A.B is saved in the file A_B.lua
-- local A_B = require("A_B")
toLuaPath modName = dquotes (flatModName modName)


-- Render simplified language as Js
renderJs sts = render (toJsM sts)

toJsM (Mod modName imports statements) = vcat [
    vcatMap toJsI imports,
    text "const" <+> flatModName modName <+> equals <+> text "{}" <> semi,
    vcatMap toJsS statements,
    text "module.exports" <+> equals <+> flatModName modName <> semi]

toJsI modName =
    text "const" <+> flatModName modName <+> equals <+>
        text "require" <> parens (toJsPath modName) <> semi

toJsS (Assign (Name [] (Id "_" _)) e) =
    toJsE e <> semi
toJsS (Assign (Name [] x) e) =
    text "const" <+> pretty x <+> equals <+> toJsE e <> semi
toJsS (Assign x e) =
    flatName x <+> equals <+> toJsE e <> semi
toJsS (Ret e) =
    text "return" <+> toJsE e <> semi
toJsS (If e th []) = vcat [
    text "if" <> parens (toJsE e) <+> text "{",
    indent 4 (vcatMap toJsS th),
    text "}"]
toJsS (If e [] th) = vcat [
    text "if" <> parens (text "!" <> parens (toJsE e)) <+> text "{",
    indent 4 (vcatMap toJsS th),
    text "}"]
toJsS (If e th el) = vcat [
    text "if" <> parens (toJsE e) <+> text "{",
    indent 4 (vcatMap toJsS th),
    text "}" <+> text "else" <+> text "{",
    indent 4 (vcatMap toJsS el),
    text "}"]

toJsE (Var x) = flatName x
toJsE (LitI i) = pretty i
toJsE (LitD d) = prettyNumeral d
toJsE (LitT t) = prettyEscaped t
toJsE (Func vs sts) = vcat [
    text "function" <> parens (commas (fmap pretty vs)) <+> text "{",
    indent 4 (vcatMap toJsS sts),
    text "}"]
toJsE (Access v indices) =
    pretty v <> foldMap' (brackets . pretty) indices
-- Add parantheses for immediate functions
toJsE (Call e@(Func _ _) es) =
    parens (toJsE e) <> parens (commas (fmap toJsE es))
toJsE (Call e es) =
    toJsE e <> parens (commas (fmap toJsE es))
toJsE (Arr es) = brackets (commas (fmap toJsE es))
toJsE (And e1 e2) =
    toJsE e1 <+> text "&&" <+> toJsE e2
toJsE (Eq e1 e2) =
    toJsE e1 <+> text "===" <+> toJsE e2

-- js needs the leading dot for local modules
toJsPath modName = dquotes ("./" <> flatModName modName)


-- Compile to simplified language
compile (ModuleDeclaration modName decls) =
    let
        compiledDecls = foldMap' compileTop decls
        imports = compileImports decls
    in Mod modName (imports ++ compiledDecls)

compileE (Variable v) = Var v
compileE (ConstructorExpression c) = Var c
compileE (FunctionApplication f e) = Call (compileE f) [compileE e]
compileE (LambdaExpression [VariablePattern v] e) =
    Func [toId v] (compileEtoS e)
compileE (LambdaExpression [p] e) =
    let
        v = prefixedId "l"
        err = Ret (mkError ("No pattern match in lambda for " <> prettyError p))
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
        err = Ret (mkError ("No pattern match for " <> prettyError (fmap fst alts)))
    in Assign (fromId v) (compileE e) : compileAlts v [err] alts
compileEtoS (LetExpression decls e) =
    foldMap' compileD decls ++ compileEtoS e
compileEtoS (IfExpression c th el) =
    [If (compileE c) (compileEtoS th) (compileEtoS el)]
compileEtoS e = [Ret (compileE e)]

compileL (Numeral n) = LitD n
compileL (Text t) = LitT t

-- Compile top level declarations
compileTop (TypeDeclaration _ _ cs) =
    fmap compileConstructor cs
compileTop (FixityDeclaration _ _ _ _) = []
compileTop other = compileD other

compileD (ExpressionDeclaration (VariablePattern x) e) =
    [Assign x (compileE e)]
compileD (ExpressionDeclaration (Wildcard w) e) =
    [Assign (fromId w) (compileE e)]
{-
Compile pattern matches in let expressions like
let
    Tuple a b = Tuple 2 3
in write (toString a)
-}
compileD (ExpressionDeclaration p pe) =
    let
        v = prefixedId "d"
        err = Ret (mkError ("No pattern match in declaration for " <> prettyError p))
        s = getAssignments v [] p
        cs = getConditions v [] p
    in Assign (fromId v) (compileE pe) : If (foldr1 And cs) [] [err] : s
compileD (TypeSignature _ _) = []
compileD (AliasDeclaration _ _) = []
compileD other = error ("compileD does not work on " ++ show other)

compileConstructor (c, []) =
    Assign c (Func [] [])
compileConstructor (c, vs) =
    let
        vars = nNewVars (length vs)
        body = Arr (fmap Var (c:fmap fromId vars))
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
getConditions _ _ p = error ("getConditions on " ++ renderError p)

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
getAssignments _ _ p = error ("getAssignments on " ++ renderError p)

descendAccess _ _ _ [] = []
descendAccess f i j (p:ps) =
    f (i ++ [j]) p ++ descendAccess f i (j + 1) ps


immediate sts = Call (Func [] sts) []


mkError s = Call (Var (fromText "Native.error")) [LitT (render s)]
mkIsArray a = Call (Var (fromText "Native.isArray")) [a]
mkSize a = Call (Var (fromText "Native.size")) [a]


vcatMap f x = vcat (fmap f x)
