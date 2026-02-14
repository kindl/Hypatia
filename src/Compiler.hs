{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import Syntax
import Data.List(transpose)
import Data.Text(Text)
import Prettyprinter(vcat, indent, (<+>),
    equals, braces, parens, brackets, semi, pretty, dquotes, hardline)
import Data.Foldable(foldMap')
import qualified Data.HashSet as Set
import Data.Data(Data, Typeable)
import Data.Generics.Uniplate.Data(universeBi, transformBi)
import Data.HashMap.Strict(fromList, lookup)
import Prelude hiding (lookup)


type Import = Name

data Mod = Mod Name [Import] [Statement]
    deriving (Show, Typeable, Data)

data Statement =
    Ret Expr
    | Assign Binding Expr
    | If Expr [Statement] [Statement]
        deriving (Show, Typeable, Data)

data Op =
    And
    | Or
    | Eq
    | Ne
    | Le
    | Ge
    | Lt
    | Gt
    | Plus
    | Minus
    | Multiply
    | Divide
    | Modulo
    | Power
    | Concat
    deriving (Show, Typeable, Data)

data Expr =
    Var Name
    | Tag Name
    | LitI Int
    | LitD Double
    | LitT Text
    | LitB Bool
    | Func [Id] [Statement]
    | Access Id [Int]
    | Call Expr [Expr]
    | Arr [Expr]
    | Op Op Expr Expr
    | Negate Expr
        deriving (Show, Typeable, Data)


-- Render simplified language as Lua
renderLua m = render (renderLuaMod m)

renderLuaMod (Mod _ imports statements) = vcat [
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

toLuaS (Assign (Name [] (Id "_" _)) e) =
    toLuaE e
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
    text "end",
    mempty]
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
toLuaE (Tag x) = flatName x
toLuaE (LitI i) = pretty i
toLuaE (LitD d) = prettyNumber d
toLuaE (LitT t) = prettyEscaped t
toLuaE (LitB True) = text "true"
toLuaE (LitB False) = text "false"
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
toLuaE (Op op e1 e2) =
    maybeParens toLuaE e1 <+> toLuaOp op <+> maybeParens toLuaE e2
toLuaE (Negate e) = "-" <> maybeParens toLuaE e

toLuaOp And = text "and"
toLuaOp Or = text "or"
toLuaOp Eq = text "=="
toLuaOp Ne = text "~="
toLuaOp Power = text "^"
toLuaOp Concat = text ".."
toLuaOp other = toOp other

toOp Lt = text "<"
toOp Gt = text ">"
toOp Le = text "<="
toOp Ge = text ">="
toOp Plus = text "+"
toOp Minus = text "-"
toOp Multiply = text "*"
toOp Divide = text "/"
toOp Modulo = text "%"
toOp other =
    error ("Bug: Operator " <> show other <> " not defined")

-- e.g. A module A.B is saved in the file A_B.lua
-- local A_B = require("A_B")
toLuaPath modName = dquotes (flatModName modName)


-- Render simplified language as Js
renderJs m = render (renderJsMod m)

renderJsMod (Mod _ imports statements) = vcat [
    vcatMap toJsI imports,
    hardline,
    mintercalate (hardline <> hardline) (fmap toJsS statements),
    hardline,
    text "export default {",
    indent 4 (mintercalate (text "," <> hardline) (fmap pretty (getIdentifiers statements))),
    text "}"
    ]

toJsI modName =
    text "import" <+> flatModName modName <+>
        text "from" <+> toJsPath modName <> semi

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
    text "}",
    mempty]
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
toJsE (Tag x) = flatName x
toJsE (LitI i) = pretty i
toJsE (LitD d) = prettyNumber d
toJsE (LitT t) = prettyEscaped t
toJsE (LitB True) = text "true"
toJsE (LitB False) = text "false"
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
toJsE (Op op e1 e2) =
    maybeParens toJsE e1 <+> toJsOp op <+> maybeParens toJsE e2
toJsE (Negate e) = "-" <> maybeParens toJsE e

toJsOp And = text "&&"
toJsOp Or = text "||"
toJsOp Eq = text "==="
toJsOp Ne = text "!=="
toJsOp Concat = text "+"
-- Operator only available in new js versions
toJsOp Power = text "**"
toJsOp other = toOp other

-- js needs the leading dot for local modules
toJsPath modName = dquotes ("./" <> flatModName modName <> ".js")

-- TODO handling of imported names:
-- If they shadow another name, use full qualified name, otherwise short name
-- For example there is both, `Array.map` and `Reader.map` they become
-- `Array_map` and `Reader_map` otherwise `Array.map` just becomes `map`.
optimize arityMap keywords (Mod modName imports statements) =
    Mod modName imports
        ((transformBi (optimizeName modName)
        . transformBi (renameKeywords keywords)
        . transformBi inlineOperators
        . optimizeApplication arityMap)
            statements)

inlineOperators (Call (Var (Name ["Native"] (Id "negate" _))) [a]) = Negate a
inlineOperators (Call (Var (Name ["Native"] (Id "and" _))) [a, b]) = Op And a b
inlineOperators (Call (Var (Name ["Native"] (Id "or" _))) [a, b]) = Op Or a b
inlineOperators (Call (Var (Name ["Native"] (Id "eq" _))) [a, b]) = Op Eq a b
inlineOperators (Call (Var (Name ["Native"] (Id "ne" _))) [a, b]) = Op Ne a b
inlineOperators (Call (Var (Name ["Native"] (Id "le" _))) [a, b]) = Op Le a b
inlineOperators (Call (Var (Name ["Native"] (Id "lt" _))) [a, b]) = Op Lt a b
inlineOperators (Call (Var (Name ["Native"] (Id "ge" _))) [a, b]) = Op Ge a b
inlineOperators (Call (Var (Name ["Native"] (Id "gt" _))) [a, b]) = Op Gt a b
inlineOperators (Call (Var (Name ["Native"] (Id "plus" _))) [a, b]) = Op Plus a b
inlineOperators (Call (Var (Name ["Native"] (Id "minus" _))) [a, b]) = Op Minus a b
inlineOperators (Call (Var (Name ["Native"] (Id "multiply" _))) [a, b]) = Op Multiply a b
inlineOperators (Call (Var (Name ["Native"] (Id "divide" _))) [a, b]) = Op Divide a b
inlineOperators (Call (Var (Name ["Native"] (Id "modulo" _))) [a, b]) = Op Modulo a b
inlineOperators (Call (Var (Name ["Native"] (Id "power" _))) [a, b]) = Op Power a b
inlineOperators (Call (Var (Name ["Native"] (Id "concat" _))) [a, b]) = Op Concat a b
inlineOperators (Var (Name ["Native"] (Id "True" _))) = LitB True
inlineOperators (Var (Name ["Native"] (Id "False" _))) = LitB False
inlineOperators (Tag (Name ["Native"] (Id "True" _))) = LitB True
inlineOperators (Tag (Name ["Native"] (Id "False" _))) = LitB False
inlineOperators e = e

optimizeApplication arityMap statements =
    fmap (removeCurryS arityMap) statements

removeCurryS arityMap (Ret e) =
    Ret (removeCurryE arityMap e)
removeCurryS arityMap (Assign n e) =
    Assign n (removeCurryE arityMap e)
removeCurryS arityMap (If e thenBranch elseBranch) =
    If (removeCurryE arityMap e)
        (fmap (removeCurryS arityMap) thenBranch)
        (fmap (removeCurryS arityMap) elseBranch)

-- A special case for unary operators
-- for example `map negate array`
-- should become `map (\v -> negate v) array`
-- and in turn an operator
removeCurryE _ e@(Var (Name ["Native"] (Id "negate" _))) =
    createPartialApplication 1 e []
removeCurryE arityMap e@(Var v) =
    case lookup v arityMap of
        Just arity | arity > 1 ->
            -- When a function is referred as a name
            -- it has to be curried as if it was applied to no arguments
            createPartialApplication arity e []
        _ -> e
removeCurryE arityMap c@(Call f originalEs) =
    case flattenCall c [] of
        (Var v, es) ->
            case lookup v arityMap of
                Just arity | arity == length es ->
                    Call (Var v) (fmap (removeCurryE arityMap) es)
                Just arity | arity > length es ->
                    createPartialApplication arity (Var v) (fmap (removeCurryE arityMap) es)
                _ ->
                    Call (removeCurryE arityMap f) (fmap (removeCurryE arityMap) originalEs)
        _ -> Call (removeCurryE arityMap f) (fmap (removeCurryE arityMap) originalEs)
removeCurryE arityMap (Func vs sts) =
    Func vs (fmap (removeCurryS arityMap) sts)
removeCurryE arityMap (Arr es) =
    Arr (fmap (removeCurryE arityMap) es)
removeCurryE arityMap (Op o e1 e2) =
    Op o (removeCurryE arityMap e1) (removeCurryE arityMap e2)
removeCurryE _ e =
    e

createPartialApplication arity e es =
    let
        vars = nNewVars (arity - length es)
        parameters = es ++ fmap (Var . fromId) vars
        body = [Ret (Call e parameters)]
    in curryFuncSts vars body

optimizeName modName n@(Name qs ident) =
    if qs == nameToList modName then Name [] ident else n

renameKeywords keywords ident@(Id i loc) =
    if elem i keywords then Id ("__" <> i) loc else ident

luaKeywords :: [Text]
luaKeywords = [
    "and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "if",
    "in", "local", "nil", "not", "or",
    "repeat", "return", "then", "true", "until", "while"
    ]

jsKeywords :: [Text]
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

-- Compiles an expression in a statement context
compileEtoS (CaseExpression (Variable (Name [] v)) alts) =
    let
        err = Ret (makeError ("No pattern match for " <> prettyError (fmap fst alts)))
    in compileAlts v [err] alts
compileEtoS (CaseExpression e alts) =
    let
        location = firstLocationInfo e
        v = locatedId location "c"
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
-- Top level function declarations are uncurried
compileTop (FunctionDeclaration x alts) =
    [Assign x (compileMultiLambda Func alts)]
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
compileD (ExpressionDeclaration p e) =
    let
        location = firstLocationInfo p
        v = locatedId location "d"
        err = Ret (makeError ("No pattern match in declaration for " <> prettyError p))
        assignments = getAssignments v [] p
        withEarlyOut = makeIf (getConditions v [] p) [] [err] <> assignments
    in Assign (fromId v) (compileE e) : withEarlyOut
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
        getTag TaggedRepresentation = [Tag c]

        parameters = nNewVars (length variables)
        body = [Ret (Arr (getTag info ++ fmap (Var . fromId) parameters))]
    in Assign c (Func parameters body)

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
makeIf cs s elseBranch = [If (foldr1 (Op And) cs) s elseBranch]

getOrMakeIds = getOrMakeIds' 0

getOrMakeIds' index (patterns:ps) =
    case extractVar patterns of
        Nothing ->
            -- Investigate if this could produce name conflicts
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
    [Op Eq (Access v i) (Tag c)]
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
    Op Eq (makeLength (Access v i)) (LitI (length ps + 1)),
    -- Compare tag
    Op Eq (Access v (i ++ [0])) (Tag c)]
    ++ descendAccess (getConditions v) i 1 ps
getConditions v i (ArrayPattern ps) =
    getConditionsArray v i ps
getConditions v i (LiteralPattern l) =
    [Op Eq (Access v i) (compileL l)]
getConditions _ _ (Wildcard _) = []
getConditions _ _ (VariablePattern _) = []
getConditions _ _ p = error ("getConditions on " ++ renderError p)

getConditionsArray v i ps =
    [makeIsArray (Access v i),
    Op Eq (makeLength (Access v i)) (LitI (length ps))]
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

flattenCall (Call f [e]) es =
    flattenCall f (e:es)
flattenCall e es = (e, es)

captureArity m =
    captureFunctionArity m
    <> captureConstructorArity m
    <> captureSignatureArity m

captureFunctionArity (ModuleDeclaration _ _ decls) =
    fromList [(x, length (fst (head alts))) | FunctionDeclaration x alts <- decls]

captureConstructorArity (ModuleDeclaration _ _ decls) =
    fromList (concat [fmap (fmap length) cs | TypeDeclaration _ _ cs <- decls])

captureSignatureArity (ModuleDeclaration _ _ decls) =
    fromList (concat [getArityFromType x ty | TypeSignature x ty <- decls])

getArityFromType x ty@(TypeArrow _ _) =
    let l = countArrows ty
    in [(x, l)]
getArityFromType x (ForAll _ ty) = getArityFromType x ty
getArityFromType _ _ = []

countArrows (ForAll _ ty) = countArrows ty
countArrows (TypeArrow _ ty) = 1 + countArrows ty
countArrows _ = 0

-- Helper for nested operators
maybeParens f e =
    case e of
        Op _ _ _ -> parens (f e)
        Negate _ -> parens (f e)
        _ -> f e
