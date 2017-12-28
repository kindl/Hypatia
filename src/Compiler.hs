module Compiler where


import Syntax
import Data.Text(Text, pack)
import Text.PrettyPrint(vcat, (<+>), (<>), ($$),
    equals, text, int, braces, parens, brackets, double, render)
import Text.PrettyPrint.HughesPJClass(pPrint)


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
    vcat [text "local" <+> pPrint x | Assign x _ <- sts]

toLuaS (Assign x e) =
    pPrint x <+> equals <+> toLuaE e
toLuaS (Imp modName) =
    text "local" <+> flatModName modName <+> equals
        <+> text "require" <+> pPrint (toPath modName)
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
    parens (text "function" <> parens (commas (fmap pPrint vs))
        $$ toLua sts $$ text "end")
toLuaE (Access v indices) =
    pPrint v <> foldMap (brackets . int . (+ 1)) indices
toLuaE (Call e es) = toLuaE e <> parens (commas (fmap toLuaE es))
toLuaE (Arr es) = braces (commas (fmap toLuaE es))

{- JavaScript -}
renderJavaScript sts = render (toJavaScriptM sts)

toJavaScriptM (Mod modName sts) =
    text "const" <+> flatModName modName <+> equals <+> text "{}" <> text ";"
        $$ vcatMap (toJavaScriptT modName) sts
        $$ text "module.exports" <+> equals <+> flatModName modName <> text ";"

toJavaScriptT modName (Assign x e) =
    flatVar (qualifyId modName x) <+> equals <+> toJavaScriptE e <> text ";"
toJavaScriptT _ s = toJavaScriptS s

toJavaScriptS (Assign x e) =
    text "const" <+> pPrint x <+> equals <+> toJavaScriptE e <> text ";"
toJavaScriptS (Imp modName) =
    text "const" <+> flatModName modName <+> equals <+>
        -- js needs the leading dot for local modules
        text "require" <> parens (pPrint ("./" ++ toPath modName)) <> text ";"
toJavaScriptS (Ret e) =
    text "return" <+> toJavaScriptE e <> text ";"
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
    parens (text "function" <> parens (commas (fmap pPrint vs))
        $$ block (vcatMap toJavaScriptS sts))
toJavaScriptE (Access v indices) =
    pPrint v <> foldMap (brackets . int) indices
toJavaScriptE (Call e es) =
    toJavaScriptE e <> parens (commas (fmap toJavaScriptE es))
toJavaScriptE (Arr es) = brackets (commas (fmap toJavaScriptE es))

vcatMap f x = vcat (fmap f x)
commas x = mintercalate (text ", ") x
block s = text "{" $$ s $$ text "}"

-- Compile to simplified language
compile (ModuleDeclaration modName decls) =
    Mod modName (makeForeigns decls ++ foldMap (compileT modName) decls)

compileE (Variable v) = Var v
compileE (ConstructorExpression c) = Var c
compileE (FunctionApplication f e) = Call (compileE f) [compileE e]
compileE (LambdaExpression [VariablePattern v] e) =
    Func [v] [Ret (compileE e)]
compileE (LambdaExpression [Wildcard] e) =
    Func [makeId "_w"] [Ret (compileE e)]
compileE (LambdaExpression [p] e) =
    Func [makeId "_v"] (compileAlt (p, e) :
        [Ret (Call (makeVar "Native.error")
            [LitT (pack ("failed pattern match lambda at " ++ locationInfoP p))])])
compileE (CaseLambdaExpression alts) =
    Func [makeId "_v"] (fmap compileAlt alts ++
        [Ret (Call (makeVar "Native.error")
            [LitT (pack ("failed pattern match case lambda at "
                ++ mintercalate " " (fmap (locationInfoP . fst) alts)))])])
compileE (LiteralExpression l) = compileL l
compileE (LetExpression decls e) =
    immediate (foldMap compileD decls ++ [Ret (compileE e)])
compileE (IfExpression c th el) =
    immediate [If (eqTrue (compileE c)) [Ret (compileE th)] [Ret (compileE el)]]
compileE (ArrayExpression es) =
    Arr (fmap compileE es)
compileE e = error ("compileE does not work on " ++ show e)

eqTrue e =
    Call (Call (makeVar "Native.eq") [makeVar "Prelude.True"]) [e]

compileL (Numeral n) = LitD n
compileL (Text t) = LitT t

compileT modName (EnumDeclaration _ _ cs) =
    fmap (compileEnum modName) cs
compileT _ (ImportDeclaration modName _ _) = [Imp modName]
compileT _ (FixityDeclaration _ _ _ _) = []
compileT _ other = compileD other

compileD (ExpressionDeclaration (VariablePattern x) e) =
    [Assign x (compileE e)]
compileD (ExpressionDeclaration Wildcard e) =
    [Assign (makeId "_w") (compileE e)]
compileD (TypeSignature _ _) = []
compileD (AliasDeclaration _ _) = []
compileD other = error ("compileD does not work on " ++ show other)

compileEnum _ (c, []) =
    Assign c (Func [] [])
compileEnum modName (c, vs) =
    let
        name = qualifyId modName c
        vars = nNewVars (length vs)
        body = Arr (fmap Var (name:fmap fromId vars))
    in Assign c (curryFunc body vars)

curryFunc = foldr (\v r -> Func [v] [Ret r])

compileAlt (p, e) =
    If (Call (Call (makeVar "Prelude.matches") [compileP p]) [makeVar "_v"])
        (getAssignments [] p ++ [Ret (compileE e)]) []

getAssignments i (VariablePattern x) =
    [Assign x (Access (makeId "_v") i)]
getAssignments i (AliasPattern x p) =
    Assign x (Access (makeId "_v") i):getAssignments i p
getAssignments i (ConstructorPattern _ ps) =
    descendAssignments i 1 ps
getAssignments i (ArrayPattern ps) =
    descendAssignments i 0 ps
getAssignments _ (LiteralPattern _) = []
getAssignments _ Wildcard = []
getAssignments _ p = error ("getAssignments on " ++ pretty p)

descendAssignments _ _ [] = []
descendAssignments i j (p:ps) =
    getAssignments (i ++ [j]) p ++ descendAssignments i (j + 1) ps


compileP (ConstructorPattern c []) = Var c
compileP (ConstructorPattern c ps) = Arr (Var c : fmap compileP ps)
compileP (VariablePattern _) = makeVar "Native.wildcard"
compileP (AliasPattern _ p) = compileP p
compileP (LiteralPattern l) = compileL l
compileP Wildcard = makeVar "Native.wildcard"
compileP (ArrayPattern ps) = Arr (fmap compileP ps)
compileP p = error ("compileP does not work on " ++ pretty p)

immediate sts = Call (Func [] sts) []

makeForeigns decls =
  let
    defs = foldMap getDefsD decls
    sigs = getSignatures decls
    foreigns = excluding defs sigs
  in Imp (fromString "Native"):fmap (Assign <*> makeNat) foreigns

getSignatures decls =
    [name | TypeSignature name _ <- decls]

makeNat = Var . qualifyId (fromString "Native")

makeVar = Var . fromString

-- e.g. A module A.B is saved in the file A.B.lua
-- local A_B = require("A.B")
toPath = pretty
