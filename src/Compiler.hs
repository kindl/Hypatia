module Compiler where


import Syntax
import Data.Text(Text, pack)
import Text.PrettyPrint(vcat, (<+>), (<>), ($$),
    equals, text, int, braces, parens, brackets, double, render)
import Text.PrettyPrint.HughesPJClass(pPrint)


data Statement
    = Ret Expr
    | Assign Name Expr
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
renderLua sts = render (toLua sts)

toLua sts =
  let
    forwards = vcatMap forwardLocals sts
    top = vcatMap toLuaS sts
  in forwards $$ top

forwardLocals (Assign x _) | isUnqualified x =
    text "local" <+> pPrint x
forwardLocals _ = mempty

toLuaS (Assign x e) =
    pPrint x <+> equals <+> toLuaE e
toLuaS (Imp modName) =
    text "local" <+>
        pPrint modName <+> equals <+> text "require" <+> pPrint (pretty modName)
toLuaS (Ret e) =
    text "return" <+> toLuaE e
toLuaS (If e th []) =
    text "if" <+> toLuaE e <+> text "then" $$ toLua th $$ text "end"
toLuaS (If e th el) =
    text "if" <+> toLuaE e <+> text "then" $$ toLua th $$ text "else" $$ toLua el $$ text "end"

toLuaE (Var x) = pPrint x
toLuaE (LitD d) = double d
toLuaE (LitT t) = text (show t)
toLuaE (Func vs sts) =
    parens (text "function" <> parens (commas (fmap pPrint vs))
        $$ toLua sts $$ text "end")
toLuaE (Access v indices) =
    pPrint v <> foldMap (\x -> brackets (int (x + 1))) indices
toLuaE (Call e es) = toLuaE e <> parens (commas (fmap toLuaE es))
toLuaE (Arr es) = braces (commas (fmap toLuaE es))

{- UnityScript -}
renderUnityScript sts = render (toUnityScript sts)

-- tail and init drop the array and return needed for lua
toUnityScript sts = vcatMap toUnityScriptT (tail (init sts))

-- top level decls have to be static so that Example.x works
toUnityScriptT (Assign (Name _ i) e) =
    text "static" <+> toUnityScriptS (Assign (Name [] i) e)
toUnityScriptT s = toUnityScriptS s

{-
This is a trick to allow recursion, otherwise:
Definition of 'Example.Vec3' depends on 'Example.Vec3' whose type could not be resolved because of a cycle.
-}
toUnityScriptS (Assign x e@(Func _ _)) =
    text "var" <+> pPrint x <+> text ": Object" <+> equals $$ toUnityScriptE e <> text ";"
toUnityScriptS (Assign x e) =
    text "var" <+> pPrint x <+> equals <+> toUnityScriptE e <> text ";"
toUnityScriptS (Imp _) =
    mempty
toUnityScriptS (Ret e) =
    text "return" <+> toUnityScriptE e <> text ";"
-- UnityScript does not have block scoping, therefore we use immediate to generate a function for each block
toUnityScriptS (If e th []) =
    text "if" <> parens (toUnityScriptE e)
        $$ block (toUnityScriptS (Ret (immediate th)))
toUnityScriptS (If e th el) =
    text "if" <> parens (toUnityScriptE e)
        $$ block (toUnityScriptS (Ret (immediate th)))
        $$ text "else" $$ block (toUnityScriptS (Ret (immediate el)))

toUnityScriptE (Var x) = pPrint x
toUnityScriptE (LitD d) = double d
toUnityScriptE (LitT t) = text (show t)
toUnityScriptE (Func vs sts) =
    parens (text "function" <> parens (commas (fmap pPrint vs))
        $$ block (vcatMap toUnityScriptS sts))
toUnityScriptE (Access v indices) =
    pPrint v <> foldMap (\x -> brackets (int x)) indices
toUnityScriptE (Call e es) = toUnityScriptE e <> parens (commas (fmap toUnityScriptE es))
toUnityScriptE (Arr es) = brackets (commas (fmap toUnityScriptE es))

vcatMap f x = vcat (fmap f x)
commas x = mintercalate (text ", ") x
block s = text "{" $$ s $$ text "}"

-- Compile to simplified language
compile (ModuleDeclaration modName decls) =
    -- TODO qualified module names
    -- A qualified module name leads to
    -- A.B.C = {} which is an error because A is undefined
    [Assign modName (Arr []), Imp (fromString "Native")]
        ++ makeForeigns modName decls ++ foldMap (compileTopD modName) decls
                    ++ [Ret (Var modName)]

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

compileTopD _ (ImportDeclaration modName _ _) = [Imp modName]
compileTopD _ (FixityDeclaration _ _ _ _) = []
compileTopD modName (EnumDeclaration _ _ cs) = fmap (compileEnum modName) cs
compileTopD modName (ExpressionDeclaration (VariablePattern x) e) =
    [Assign (qualifyId modName x) (compileE e)]
compileTopD _ other = compileD other

compileD (ExpressionDeclaration (VariablePattern x) e) =
    [Assign (fromId x) (compileE e)]
compileD (ExpressionDeclaration Wildcard e) =
    [Assign (fromString "_w") (compileE e)]
compileD (TypeSignature _ _) = []
compileD (AliasDeclaration _ _) = []
compileD other = error ("compileD does not work on " ++ show other)

compileEnum modName (c, []) =
    Assign (qualifyId modName c) (Func [] [])
compileEnum modName (c, vs) =
    let
        name = qualifyId modName c
        vars = nNewVars (length vs)
        body = Arr (fmap Var (name:fmap fromId vars))
    in Assign name (curryFunc body vars)

curryFunc e [] = e
curryFunc e (v:vs) = Func [v] [Ret (curryFunc e vs)]

compileAlt (p, e) =
    If (Call (Call (makeVar "Prelude.matches") [compileP p]) [makeVar "_v"])
        (getAssignments [] p ++ [Ret (compileE e)]) []

getAssignments i (VariablePattern x) =
    [Assign (fromId x) (Access (makeId "_v") i)]
getAssignments i (AliasPattern x p) =
    Assign (fromId x) (Access (makeId "_v") i):getAssignments i p
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

makeForeigns modName decls =
  let
    defs = foldMap getDefsD decls
    sigs = getSignatures decls
    foreigns = excluding defs sigs
  in
    fmap (\x -> Assign (qualifyId modName x) (makeNat x)) foreigns

getSignatures decls =
    [name | TypeSignature name _ <- decls]

makeNat = Var . qualifyId (fromString "Native")

makeVar = Var . fromString
