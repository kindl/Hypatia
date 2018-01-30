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
toLuaE (And e1 e2) = toLuaE e1 <+> text "and" <+> toLuaE e2

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
toJavaScriptE (And e1 e2) = toJavaScriptE e1 <+> text "&&" <+> toJavaScriptE e2

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
    If (getMatchings p)
        (getAssignments [] p ++ [Ret (compileE e)]) []


getMatchings p = foldl1 And (case getMatching [] p of
        [] -> [makeVar "true"]
        xs -> xs)

getMatching _ Wildcard = []
getMatching _ (VariablePattern _) = []
getMatching i (AliasPattern _ p) =
    getMatching i p
getMatching i (ConstructorPattern c []) =
    [Call (Call (makeVar "Native.eq")
        [Access (makeId "_v") i]) [Var c]]
getMatching i (ConstructorPattern c ps) =
    [Call (makeVar "Native.isArray") [Access (makeId "_v") i],
        Call (Call (makeVar "Native.eq")
            [Access (makeId "_v") (i ++ [0])]) [Var c]]
                ++ descendAccess getMatching i 1 ps
getMatching i (ArrayPattern ps) =
    descendAccess getMatching i 0 ps
getMatching i (LiteralPattern l) =
    [Call (Call (makeVar "Native.eq")
        [Access (makeId "_v") i]) [compileL l]]
getMatching _ p = error ("getMatching on " ++ pretty p)


getAssignments i (VariablePattern x) =
    [Assign x (Access (makeId "_v") i)]
getAssignments i (AliasPattern x p) =
    Assign x (Access (makeId "_v") i):getAssignments i p
getAssignments i (ConstructorPattern _ ps) =
    descendAccess getAssignments i 1 ps
getAssignments i (ArrayPattern ps) =
    descendAccess getAssignments i 0 ps
getAssignments _ (LiteralPattern _) = []
getAssignments _ Wildcard = []
getAssignments _ p = error ("getAssignments on " ++ pretty p)

descendAccess _ _ _ [] = []
descendAccess f i j (p:ps) =
    f (i ++ [j]) p ++ descendAccess f i (j + 1) ps


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
