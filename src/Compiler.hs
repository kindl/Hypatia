{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Compiler where

import Syntax
import Data.List(transpose)
import Data.Text(Text, pack)
import Prettyprinter(vcat, indent, (<+>),
    equals, braces, parens, brackets, semi, pretty, dquotes, hardline)
import Data.Foldable(foldMap')
import qualified Data.HashSet as Set
import Data.Data(Data, Typeable)
import Data.Generics.Uniplate.Data(universeBi, transformBi)


type Import = Name

data Mod = Mod Name [Import] [Statement]
    deriving (Show, Typeable, Data)

data Statement
    = Ret Expr
    | Assign Binding Expr
    | If Expr [Statement] [Statement]
        deriving (Show, Typeable, Data)

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
        deriving (Show, Typeable, Data)


-- Render simplified language as Lua
renderLua m = render (toLuaM (optimizeNames luaKeywords m))

toLuaM (Mod _ imports statements) = vcat [
    vcatMap toLuaI imports,
    hardline,
    mintercalate (hardline <> hardline) (fmap toLuaS statements),
    hardline,
    text "return {",
    indent 4 (mintercalate (text "," <> hardline) (fmap exports (getIdentifiers statements))),
    text "}"
    ]

exports x = pretty x <+> text "=" <+> pretty x

getIdentifiers statements =
    [i | Assign (Name _ (Id i _)) _ <- statements, not (isWildcard i)]

isWildcard i = i == "_"

toLuaI modName =
    text "local" <+> flatModName modName <+> equals
        <+> text "require" <+> toLuaPath modName

-- local functions need a forward declaration for recursion
-- Otherwise `local fix = function(f) return f(fix(f)) end`
-- would result in an error because `fix` is undefined
-- TODO decide if a forward declaration is necessary without looking
-- at all names appearing in the function body. Maybe the result of
-- sorting LetExpressions could be reused.
toLuaS (Assign (Name [] x) e) =
    if elem x [ident | Name [] ident <- universeBi e]
        then vcat [text "local" <+> pretty x, pretty x <+> equals <+> toLuaE e]
        else text "local" <+> pretty x <+> equals <+> toLuaE e
toLuaS (Assign x _) =
    error ("Unexpected qualified name " <> renderError x)
toLuaS (Ret e) =
    text "return" <+> toLuaE e
toLuaS (If e th []) = vcat [
    text "if" <+> toLuaE e <+> text "then",
    indent 4 (vcatMap toLuaS th),
    text "end"]
-- This shortcut exists for pattern matches in let expressions
toLuaS (If e [] th) = vcat [
    text "if not" <+> parens (toLuaE e) <+> text "then",
    indent 4 (vcatMap toLuaS th),
    text "end"]
-- Render elseif
toLuaS (If e th [s@(If _ _ _)]) = vcat [
    text "if" <+> toLuaE e <+> text "then",
    indent 4 (vcatMap toLuaS th),
    text "else" <> toLuaS s]
toLuaS (If e th el) = vcat [
    text "if" <+> toLuaE e <+> text "then",
    indent 4 (vcatMap toLuaS th),
    text "else",
    indent 4 (vcatMap toLuaS el),
    text "end"]

toLuaE (Var x) = flatName x
toLuaE (LitI i) = pretty i
toLuaE (LitD d) = prettyNumber d
toLuaE (LitT t) = prettyEscaped t
toLuaE (Func [] []) = text "function() end"
toLuaE (Func variables statements) = vcat [
    text "function" <> parens (commas (fmap pretty variables)),
    indent 4 (vcatMap toLuaS statements),
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
renderJs m = render (toJsM (optimizeNames jsKeywords m))

toJsM (Mod _ imports statements) = vcat [
    vcatMap toJsI imports,
    hardline,
    mintercalate (hardline <> hardline) (fmap toJsS statements),
    hardline,
    text "module.exports = {",
    indent 4 (mintercalate (text "," <> hardline) (fmap pretty (getIdentifiers statements))),
    text "}"
    ]

toJsI modName =
    text "const" <+> flatModName modName <+> equals <+>
        text "require" <> parens (toJsPath modName) <> semi

toJsS (Assign (Name [] (Id "_" _)) e) =
    toJsE e <> semi
toJsS (Assign (Name [] x) e) =
    text "const" <+> pretty x <+> equals <+> toJsE e <> semi
toJsS (Assign x _) =
    error ("Unexpected qualified name " <> renderError x)
toJsS (Ret e) =
    text "return" <+> toJsE e <> semi
toJsS (If e th []) = vcat [
    text "if" <+> parens (toJsE e) <+> text "{",
    indent 4 (vcatMap toJsS th),
    text "}"]
-- This shortcut exists for pattern matches in let expressions
toJsS (If e [] th) = vcat [
    text "if" <+> parens (text "!" <> parens (toJsE e)) <+> text "{",
    indent 4 (vcatMap toJsS th),
    text "}"]
-- Render else if
toJsS (If e th [s@(If _ _ _)]) = vcat [
    text "if" <+> parens (toJsE e) <+> text "{",
    indent 4 (vcatMap toJsS th),
    text "}" <+> text "else" <+> toJsS s]
toJsS (If e th el) = vcat [
    text "if" <+> parens (toJsE e) <+> text "{",
    indent 4 (vcatMap toJsS th),
    text "}" <+> text "else" <+> text "{",
    indent 4 (vcatMap toJsS el),
    text "}"]

toJsE (Var x) = flatName x
toJsE (LitI i) = pretty i
toJsE (LitD d) = prettyNumber d
toJsE (LitT t) = prettyEscaped t
toJsE (Func [] []) = text "function() {}"
toJsE (Func variables statements) = vcat [
    text "function" <> parens (commas (fmap pretty variables)) <+> text "{",
    indent 4 (vcatMap toJsS statements),
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

-- TODO handling of imported names:
-- If they shadow another name, use full qualified name, otherwise short name
-- For example there is both, `Array.map` and `Reader.map` they become
-- `Array_map` and `Reader_map` otherwise `Array.map` just becomes `map`.
optimizeNames keywords (Mod modName imports statements) =
    let
        optimizeName n@(Name qs ident) =
            if qs == nameToList modName then Name [] ident else n
    in Mod modName imports ((transformBi optimizeName . transformBi (renameKeywords keywords)) statements)

renameKeywords keywords ident@(Id i loc) =
    if elem i keywords then Id ("__" <> i) loc else ident

luaKeywords = [
    "and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "if",
    "in", "local", "nil", "not", "or",
    "repeat", "return", "then", "true", "until", "while"
    ]

jsKeywords = [
    "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete",
    "do", "else", "export", "extends", "false", "finally", "for", "function", "if",
    "import", "in", "instanceof", "new", "null", "return", "super", "switch",
    "this", "throw", "true", "try", "typeof", "var", "void", "while", "with",
    "let", "static", "yield", "await",
    "enum", "implements", "interface", "package", "private", "protected", "public",
    -- These are not reserved words, but should not be overriden in strict mode
    "eval", "arguments"
    ]

-- Compile to simplified language
compile m =
    let
        compiledDecls = foldMap' compileTop (getDecls m)
        imports = importedModules m
    in Mod (getName m) (Set.toList imports) compiledDecls

compileE (Variable v) =
    Var v
compileE (ConstructorExpression c) =
    Var c
compileE (FunctionApplication f e) =
    Call (compileE f) [compileE e]
compileE (LambdaExpression [p] e) =
    compileLambda Func [p] e
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

compileMultiLambda f alts =
    let
        ps = fmap fst alts
        vs = getOrMakeIds (transpose ps)
        err = Ret (makeError ("No pattern match in function for " <> prettyError ps))
        sts = compileMultiAlts vs [err] alts
    in f vs sts

compileLambda f ps e =
    let
        vs = getOrMakeIds (fmap return ps)
        err = Ret (makeError ("No pattern match in lambda for " <> prettyError ps))
        sts = compileMultiAlts vs [err] [(ps, e)] 
    in f vs sts

{-
Compiles an expression in a statement context
TODO investigate
Compiling an expressions to a statement seemed to be a good way of saving immediate functions
however nested case expressions lead to problems e.g. multiple defined local _v
-}
compileEtoS (CaseExpression (Variable (Name [] v)) alts) =
    let
        err = Ret (makeError ("No pattern match for " <> prettyError (fmap fst alts)))
    in compileAlts v [err] alts
compileEtoS (CaseExpression e alts) =
    let
        v = prefixedId builtinLocation "c"
        err = Ret (makeError ("No pattern match for " <> prettyError (fmap fst alts)))
    in Assign (fromId v) (compileE e) : compileAlts v [err] alts
compileEtoS (LetExpression decls e) =
    foldMap' compileD decls ++ compileEtoS e
compileEtoS (IfExpression c th el) =
    [If (compileE c) (compileEtoS th) (compileEtoS el)]
compileEtoS e = [Ret (compileE e)]

compileL (Number n) = LitD n
compileL (Text t) = LitT t

-- Compile top level declarations
compileTop (TypeDeclaration _ _ [c]) =
    [compileConstructor ArrayRepresentation c]
compileTop (TypeDeclaration _ _ cs) =
    fmap (compileConstructor TaggedRepresentation) cs
compileTop (FixityDeclaration _ _ _ _) = []
compileTop other = compileD other

compileD (FunctionDeclaration x alts) =
    [Assign x (compileMultiLambda curryFuncSts alts)]
compileD (ExpressionDeclaration (VariablePattern x) e) =
    [Assign x (compileE e)]
compileD (ExpressionDeclaration (Wildcard w) e) =
    [Assign (fromId w) (compileE e)]
{-
Compile pattern matches in let expressions like
let
    Tuple2 a b = Tuple2 2 3
in print (toString a)
-}
compileD (ExpressionDeclaration p pe) =
    let
        v = prefixedId builtinLocation "d"
        err = Ret (makeError ("No pattern match in declaration for " <> prettyError p))
        assignments = getAssignments v [] p
        withEarlyOut = makeIf (getConditions v [] p) [] [err] <> assignments
    in Assign (fromId v) (compileE pe) : withEarlyOut
compileD (TypeSignature _ _) = []
compileD (AliasDeclaration _ _) = []
compileD other = error ("compileD does not work on " ++ show other)

compileConstructor _ (c, []) =
    Assign c (Func [] [])
compileConstructor ArrayRepresentation (c, [_]) =
    Assign c (Var (fromText "Native.unsafeCoerce"))
compileConstructor info (c, variables) =
    let
        getTag ArrayRepresentation = []
        getTag TaggedRepresentation = [c]

        parameters = nNewVars (length variables)
        body = [Ret (Arr (fmap Var (getTag info ++ fmap fromId parameters)))]
    in Assign c (curryFuncSts parameters body)

curryFuncSts [v] s = Func [v] s
curryFuncSts (v:vs) s = Func [v] [Ret (curryFuncSts vs s)]

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
    let
        assignments = getAssignments v [] p
        conditions = getConditions v [] p
        s = assignments ++ compileEtoS e
    in makeIf conditions s rest)

compileMultiAlts vs = foldr (\(ps, e) rest ->
    let
        assignments = concat (zipWith (\v p -> getAssignments v [] p) vs ps)
        conditions = concat (zipWith (\v p -> getConditions v [] p) vs ps)
        s = assignments ++ compileEtoS e
    in makeIf conditions s rest)

makeIf [] s _ = s
makeIf cs s elseBranch = [If (foldr1 And cs) s elseBranch]

getOrMakeIds = getOrMakeIds' 0

getOrMakeIds' index (patterns:ps) =
    case extractVar patterns of
        Nothing -> 
            prefixedId builtinLocation ("l" <> intToText index) : getOrMakeIds' (index + 1) ps
        Just v ->
            toId v : getOrMakeIds'(index + 1) ps
getOrMakeIds' _ [] = []

extractVar [VariablePattern x] = Just x
extractVar (VariablePattern x : ps) =
    case extractVar ps of
        Just v | v == x -> Just v
        _ -> Nothing
extractVar _ = Nothing

getConditions v i (AliasPattern _ p) =
    getConditions v i p
getConditions v i (ConstructorPattern _ c []) =
    [Eq (Access v i) (Var c)]
-- Constructors with only one variable are not wrapped in an array
getConditions v i (ConstructorPattern ArrayRepresentation _ [p]) =
    getConditions v i p
-- Compare as array for constructors without tag
getConditions v i (ConstructorPattern ArrayRepresentation _ ps) =
    getConditionsArray v i ps
-- Compare as array and also the tag for constructors with tag
getConditions v i (ConstructorPattern TaggedRepresentation c ps) =
    [makeIsArray (Access v i),
    -- + 1 to account for length of tag
    Eq (makeLength (Access v i)) (LitI (length ps + 1)),
    -- Compare tag
    Eq (Access v (i ++ [0])) (Var c)]
    ++ descendAccess (getConditions v) i 1 ps
getConditions v i (ArrayPattern ps) =
    getConditionsArray v i ps
getConditions v i (LiteralPattern l) =
    [Eq (Access v i) (compileL l)]
getConditions _ _ (Wildcard _) = []
getConditions _ _ (VariablePattern _) = []
getConditions _ _ p = error ("getConditions on " ++ renderError p)

getConditionsArray v i ps =
    [makeIsArray (Access v i),
    Eq (makeLength (Access v i)) (LitI (length ps))]
    ++ descendAccess (getConditions v) i 0 ps

getAssignments v [] (VariablePattern (Name [] x)) | v == x =
    []
getAssignments v i (VariablePattern x) =
    [Assign x (Access v i)]
getAssignments v i (AliasPattern x p) =
    Assign x (Access v i):getAssignments v i p
-- Identity constructor
getAssignments v i (ConstructorPattern ArrayRepresentation _ [p]) =
    getAssignments v i p
-- Untagged constructor
getAssignments v i (ConstructorPattern ArrayRepresentation _ ps) =
    descendAccess (getAssignments v) i 0 ps
-- Tagged constructor
getAssignments v i (ConstructorPattern TaggedRepresentation _ ps) =
    descendAccess (getAssignments v) i 1 ps
getAssignments v i (ArrayPattern ps) =
    descendAccess (getAssignments v) i 0 ps
getAssignments _ _ (LiteralPattern _) = []
getAssignments _ _ (Wildcard _) = []
getAssignments _ _ p = error ("getAssignments on " ++ renderError p)

descendAccess _ _ _ [] = []
descendAccess f i j (p:ps) =
    f (i ++ [j]) p ++ descendAccess f i (j + 1) ps


immediate statements = Call (Func [] statements) []


makeError s = Call (Var (fromText "Native.error")) [LitT (render s)]
makeIsArray a = Call (Var (fromText "Native.isArray")) [a]
makeLength a = Call (Var (fromText "Native.length")) [a]


vcatMap f x = vcat (fmap f x)
