module Transpiler where


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
    text "var" <+> pPrint x <+> text ": Object" <+> equals <+> toUnityScriptE e <> text ";"
toUnityScriptS (Assign x e) =
    text "var" <+> pPrint x <+> equals <+> toUnityScriptE e <> text ";"
toUnityScriptS (Imp _) =
    mempty
toUnityScriptS (Ret e) =
    text "return" <+> toUnityScriptE e <> text ";"
toUnityScriptS (If e th []) =
    text "if" <> parens (toUnityScriptE e) $$ block (vcatMap toUnityScriptS th)
toUnityScriptS (If e th el) =
    text "if" <> parens (toUnityScriptE e) $$ block (vcatMap toUnityScriptS th) $$ text "else" $$ block (vcatMap toUnityScriptS el)

toUnityScriptE (Var x) = pPrint x
toUnityScriptE (LitD d) = double d
toUnityScriptE (LitT t) = text (show t)
toUnityScriptE (Func vs sts) =
    parens (text "function" <> parens (commas (fmap pPrint vs))
        $$ block (vcatMap toUnityScriptS sts))
toUnityScriptE (Access v indices) =
    pPrint v <> foldMap (\x -> brackets (int (x + 1))) indices
toUnityScriptE (Call e es) = toUnityScriptE e <> parens (commas (fmap toUnityScriptE es))
toUnityScriptE (Arr es) = brackets (commas (fmap toUnityScriptE es))

vcatMap f x = vcat (fmap f x)
commas x = mintercalate (text ", ") x
block s = text "{" $$ s $$ text "}"

-- Transpile to simplified language
transpile (ModuleDeclaration modName decls) =
    -- TODO qualified module names
    -- A qualified module name leads to
    -- A.B.C = {} which is an error because A is undefined
    [Assign modName (Arr []), Imp (fromString "Native")]
        ++ makeForeigns modName decls ++ foldMap (transpileTopD modName) decls
                    ++ [Ret (Var modName)]

transpileE (Variable v) = Var v
transpileE (ConstructorExpression c) = Var c
transpileE (FunctionApplication f e) = Call (transpileE f) [transpileE e]
transpileE (LambdaExpression [VariablePattern v] e) =
    Func [v] [Ret (transpileE e)]
transpileE (LambdaExpression [Wildcard] e) =
    Func [makeId "_w"] [Ret (transpileE e)]
transpileE (LambdaExpression [p] e) =
    Func [makeId "_v"] (transpileAlt (p, e) :
        [Ret (Call (makeVar "error") [LitT (pack "failed pattern match lambda")])])
transpileE (CaseLambdaExpression alts) =
    Func [makeId "_v"] (fmap transpileAlt alts ++
        [Ret (Call (makeVar "error") [LitT (pack "failed pattern match case lambda")])])
transpileE (LiteralExpression l) = transpileL l
transpileE (LetExpression decls e) =
    immediate (foldMap transpileD decls ++ [Ret (transpileE e)])
transpileE (IfExpression c th el) =
    immediate [If (eqTrue (transpileE c)) [Ret (transpileE th)] [Ret (transpileE el)]]
transpileE (ArrayExpression es) =
    Arr (fmap transpileE es)
transpileE e = error ("transpileE does not work on " ++ show e)

eqTrue e =
    Call (Call (makeVar "Native.eq") [makeVar "Prelude.True"]) [e]

transpileL (Numeral n) = LitD n
transpileL (Text t) = LitT t

transpileTopD _ (ImportDeclaration modName _) = [Imp modName]
transpileTopD _ (FixityDeclaration _ _ _ _) = []
transpileTopD modName (EnumDeclaration _ _ cs) = fmap (transpileEnum modName) cs
transpileTopD modName (ExpressionDeclaration (VariablePattern x) e) =
    [Assign (qualifyId modName x) (transpileE e)]
transpileTopD _ other = transpileD other

transpileD (ExpressionDeclaration (VariablePattern x) e) =
    [Assign (fromId x) (transpileE e)]
transpileD (ExpressionDeclaration Wildcard e) =
    [Assign (fromString "_w") (transpileE e)]
transpileD (TypeSignature _ _) = []
transpileD (AliasDeclaration _ _) = []
transpileD other = error ("transpileD does not work on " ++ show other)

transpileEnum modName (c, []) =
    Assign (qualifyId modName c) (Func [] [])
transpileEnum modName (c, vs) =
    let
        name = qualifyId modName c
        vars = nNewVars (length vs)
        body = Arr (fmap Var (name:fmap fromId vars))
    in Assign name (curryFunc body vars)

curryFunc e [] = e
curryFunc e (v:vs) = Func [v] [Ret (curryFunc e vs)]

transpileAlt (p, e) =
    If (Call (Call (makeVar "Prelude.matches") [signat p]) [makeVar "_v"])
        (getAssignments [] p ++ [Ret (transpileE e)]) []

getAssignments i (VariablePattern x) =
    [Assign (fromId x) (Access (makeId "_v") i)]
getAssignments i (AliasPattern x p) =
    Assign (fromId x) (Access (makeId "_v") i):getAssignments i p
getAssignments i (ConstructorPattern _ ps) =
    descAssignments i 1 ps
getAssignments i (ArrayPattern ps) =
    descAssignments i 0 ps
getAssignments _ (LiteralPattern _) = []
getAssignments _ Wildcard = []
getAssignments _ p = error ("getAssignments on " ++ pretty p)

descAssignments _ _ [] = []
descAssignments i j (p:ps) =
    getAssignments (i ++ [j]) p ++ descAssignments i (j + 1) ps


signat (ConstructorPattern c []) = Var c
signat (ConstructorPattern c ps) = Arr (Var c : fmap signat ps)
signat (VariablePattern _) = makeVar "Native.wildcard"
signat (AliasPattern _ p) = signat p
signat (LiteralPattern l) = transpileL l
signat Wildcard = makeVar "Native.wildcard"
signat (ArrayPattern ps) = Arr (fmap signat ps)
signat p = error ("signat does not work on " ++ pretty p)

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
