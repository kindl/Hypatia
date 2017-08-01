module Parser where

import Syntax
import Data.Text(pack)
import Data.Maybe(fromMaybe)
import Control.Applicative((<|>), some, many, optional)
import Control.Monad(unless, mfilter, guard)
import Control.Monad.Trans.State(StateT(..), runStateT)
import Control.Monad.Trans.Class(lift)
import Lexer hiding (varsym, qvarsym, qconid,
    conid, varid, qvarid, literal, modid, satisfy, float)


{-
This module turns a list of lexemes into a syntax tree
-}
parse path s = do
    lexemes <- lexlex path s
    (m, rest) <- runStateT modul lexemes
    unless (null rest) (throwString ("Could not parse until end. Next lexeme is: " ++ prettyLocated (head rest)))
    return m

parseFile path = do
    str <- readFile path
    fromEitherM (parse path str)

parseString s = parse "" s

prettyLocated (LocatedLexeme l pos _) = show l ++ " " ++ pretty pos

-- get the next lexeme
next = StateT muncons
muncons [] = throwString "Unexpected end"
muncons (x:xs) = return (x, xs)

satisfy predicate = mfilter predicate next

-- get a lexeme that equals this string
token str = do
    satisfy (\x -> case extractLexeme x of
        Reserved s -> pack str == s
        -- for token "-"
        Varsym s -> pack str == s
        -- for token "_"
        Varid s -> pack str == s
        _ -> False)
    return str

-- some combinators for parens for readability
parenthesized p = token "(" *> p <* token ")"
curlyBraces p = token "{" *> p <* token "}"
bracketed p = token "[" *> p <* token "]"

{- Module -}
modul = do
    name <- optional (token "module" *> modid <* token ";")
    b <- body
    optional (token ";")
    return (ModuleDeclaration (fromMaybe (fromString "Scratch") name) b)
body = impAndTopdecls <|> impdecls <|> topdecls

{- Declarations -}
impAndTopdecls = do
    is <- impdecls
    token ";"
    ts <- topdecls
    return (is ++ ts)

impdecls = sepBy1 impdecl (token ";")
impdecl = do
    token "import"
    name <- modid
    spec <- optional impspec
    return (ImportDeclaration name spec)

impspec = parenthesized (sepBy spec (token ","))

topdecls = sepBy topdecl (token ";")
topdecl = enumDeclaration <|> aliasDeclaration <|> decl
enumDeclaration = do
    token "enum"
    name <- con
    variables <- many var
    constructors <- optional (token "=" *> constrs)
    return (EnumDeclaration name variables (concat constructors))
aliasDeclaration = do
    token "alias"
    alias <- con
    token "="
    val <- otype
    return (AliasDeclaration alias val)
    
decls = curlyBraces (sepBy decl (token ";"))
decl = typeSignature <|> fixityDeclaration
    <|> operatorDeclaration <|> functionDeclaration
    <|> expressionDeclaration
    
typeSignature = do
    v <- var
    token ":"
    t <- qtype
    return (TypeSignature v t)
fixityDeclaration = do
    f <- fixity
    i <- integer
    o <- varsym
    a <- al
    return (FixityDeclaration (stringToFixity f) i o a)
functionDeclaration = do
    v <- var
    ps <- some apat
    r <- rhs
    return (FunctionDeclaration v ps r)
operatorDeclaration = do
    p1 <- lpat
    o <- varsym
    p2 <- lpat
    r <- rhs
    return (FunctionDeclaration o [p1, p2] r)
expressionDeclaration = do
    p <- lpat
    r <- rhs
    return (ExpressionDeclaration p r)

rhs = do
    token "="
    exprWithWhere
exprWithWhere = do
    e <- expr
    ws <- optional (token "where" *> decls)
    return (maybe e (flip LetExpression e) ws)

fixity = token "infixl" <|> token "infixr" <|> token "infix"

{- Types -}
qtype = forall <|> otype
otype = typeArrow <|> typeOperator <|> btype
forall = do
    token "forall"
    ids <- some varid
    token "."
    t <- otype
    return (ForAll ids t)
typeArrow = do
    b <- btype
    token "->"
    t <- otype
    return (TypeArrow b t)
typeOperator = do
    b <- btype
    o <- qvarsym
    t <- otype
    return (TypeInfixOperator b o t)

btype = fmap (foldl1 TypeApplication) (some atype)

atype = typeConstructor <|> typeVariable <|> parenthesizedType
typeConstructor = fmap TypeConstructor qcon
typeVariable = fmap TypeVariable var
parenthesizedType = fmap ParenthesizedType (parenthesized qtype)

constrs = sepBy1 constr (token "|")
constr = do
    c <- con
    ts <- many atype
    return (c, ts)

{- Expressions -}
expr = typeAnnotation <|> infixexpr
typeAnnotation = do
    _ <- infixexpr
    token ":"
    _ <- qtype
    fail "Parsed type annotation"
--  return (TypeAnnotation e t)
infixexpr = infixOperator <|> prefixNegation <|> lexpr
infixOperator = do
    l <- lexpr
    o <- qvarsym
    r <- infixexpr
    return (InfixOperator l o r)
prefixNegation = fmap PrefixNegation (token "-" *> infixexpr)
 
lexpr = caseLambdaExpression <|> lambdaExpression <|> letExpression
    <|> ifExpression <|> caseExpression <|> fexpr
caseLambdaExpression = do
    token "fun"
    token "case"
    als <- alts
    return (CaseLambdaExpression als)
lambdaExpression = do
    token "fun"
    ps <- some apat
    token "->"
    e <- expr
    return (LambdaExpression ps e)
letExpression = do
    token "let"
    ds <- decls
    token "in"
    e <- expr
    return (LetExpression ds e)
ifExpression = do
    token "if"
    c <- expr
    optional (token ";")
    token "then"
    t <- expr
    optional (token ";")
    token "else"
    e <- expr
    return (IfExpression c t e)
caseExpression = do
    token "case"
    e <- expr
    token "of"
    als <- alts
    return (CaseExpression e als)

fexpr = fmap (foldl1 FunctionApplication) (some aexpr)
 
aexpr = variable <|> constructorExpression <|> literalExpression
    <|> parenthesizedExpression <|> listExpression
variable = fmap Variable qvar
constructorExpression = fmap ConstructorExpression qcon
literalExpression = fmap LiteralExpression literal
parenthesizedExpression = fmap ParenthesizedExpression (parenthesized expr)
listExpression = fmap ArrayExpression (bracketed (sepBy expr (token ",")))

alts = curlyBraces (sepBy alt (token ";"))
alt = do
    p <- pat
    token "->"
    e <- exprWithWhere
    return (p, e)

{- Patterns -}
pat = patternInfixOperator <|> lpat
patternInfixOperator = do
    l <- lpat
    o <- qvarsym
    r <- pat
    return (PatternInfixOperator l o r)
 
lpat = negativePattern <|> constructorPattern <|> apat
negativePattern = do
    token "-"
    l <- fmap fromIntegral integer <|> float
    return (LiteralPattern (Numeral (-l)))
constructorPattern = do
    c <- qcon
    ps <- some apat
    return (ConstructorPattern c ps)

apat = wildcard <|> variablePattern
    <|> constructor <|> literalPattern
    <|> parenthesizedPattern <|> arrayPattern
{-
TODO this should be apat and then var but it is left recursive
asPattern = do
    v <- var
    token "as"
    p <- apat
    return (AliasPattern v p)
-}
variablePattern = fmap VariablePattern var
constructor = do
    c <- qcon
    return (ConstructorPattern c [])
literalPattern = fmap LiteralPattern literal
wildcard = token "_" *> return Wildcard
parenthesizedPattern = fmap ParenthesizedPattern (parenthesized pat)
arrayPattern = fmap ArrayPattern (bracketed (sepBy pat (token ",")))

{-
Read the following like this example:
A variable can be a varid like x
or a varsym like (+)
-}
al = varid <|> conid
spec = varid <|> conid <|> parenthesized varsym
var = varid <|> parenthesized varsym
qvar = qvarid <|> parenthesized qvarsym
con = conid <|> parenthesized varsym
qcon = qconid <|> parenthesized qvarsym

{-
Read the following like this example:
A qvarid can be a qualified variable like List.map
or a normal variable identifier like map
-}

isVarsym (Varsym _) = True
isVarsym _ = False

isVarid (Varid _) = True
isVarid _ = False

isConid (Conid _) = True
isConid _ = False

parseId p = do
    n <- parseName p
    case n of
        Name [] id -> return id
        _ -> lift (throwString "Id")
parseName p = do
    n <- next
    let l = extractLexeme n
    guard (p l)
    return (fromText (extractLocation n) (extractText l))

varsym = parseId isVarsym
qvarsym = parseName isVarsym
varid = parseId isVarid
qvarid = parseName isVarid
conid = parseId isConid
qconid = parseName isConid
modid = qconid

float = do
    l <- fmap extractLexeme next
    case l of
        Double d -> return d
        _ -> lift (throwString "")
integer = do
    l <- fmap extractLexeme next
    case l of
        Integer d -> return d
        _ -> lift (throwString "")
string = do
    l <- fmap extractLexeme next
    case l of
        String d -> return d
        _ -> lift (throwString "")
literal = fmap (Numeral . fromIntegral) integer <|> fmap Numeral float <|> fmap Text string
