module Parser where

import Syntax
import Data.Text(pack)
import Data.Maybe(fromMaybe)
import Data.Functor(($>), void)
import Control.Applicative((<|>), some, many, optional)
import Control.Monad(mfilter, mzero)
import Control.Monad.Trans.State(StateT(..), runStateT)
import Lexer hiding (varsym, qvarsym, qconid,
    conid, varid, qvarid, literal, modid, satisfy, float)


{-
This module turns a list of lexemes into a syntax tree
-}
parse path s = do
    lexemes <- lexlex path s
    case runStateT modul lexemes of
        Nothing -> Left "Parse error at the beginning"
        Just (result, []) -> return result
        Just (_, rest:_) -> Left ("Could not parse until end. Next lexeme is: " ++ prettyLocated rest)

parseFile path = do
    str <- readFile path
    fromEitherM (parse path str)

parseString s = parse "" s

-- get the next lexeme
next = StateT muncons
muncons [] = mzero
muncons (x:xs) = return (x, xs)

satisfy predicate = mfilter predicate next

-- get a lexeme that equals this string
token str = void (satisfy (\x -> case extractLexeme x of
    Reserved s -> pack str == s
    -- for token "-"
    Varsym s -> pack str == s
    -- for token "_"
    Varid s -> pack str == s
    _ -> False))

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
    imps <- optional impspec
    asAlias <- optional (token "as" *> modid)
    return (ImportDeclaration name imps asAlias)

impspec = parenthesized (sepBy spec (token ","))

topdecls = sepBy topdecl (token ";")
topdecl = typeDeclaration <|> aliasDeclaration <|> decl
typeDeclaration = do
    token "type"
    name <- con
    variables <- many var
    constructors <- optional (token "=" *> constrs)
    return (TypeDeclaration name variables (concat constructors))
aliasDeclaration = do
    token "alias"
    alias <- conid
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
    return (FixityDeclaration f i o a)
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

fixity = token "infixl" $> LeftAssociative
    <|> token "infixr" $> RightAssociative
    <|> token "infix" $> None

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
    fail "Parsed type annotation which is not supported yet"
--  return (TypeAnnotation e t)
infixexpr = infixOperator <|> prefixNegation <|> lexpr
infixOperator = do
    l <- lexpr
    o <- qvarsym
    r <- infixexpr
    return (InfixOperator l o r)
prefixNegation = fmap PrefixNegation (token "-" *> infixexpr)
 
lexpr = lambdaExpression <|> letExpression
    <|> ifExpression <|> caseExpression <|> fexpr
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
wildcard = token "_" $> Wildcard
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

fromVarsym (Varsym v) = return v
fromVarsym _ = mzero

fromVarid (Varid v) = return v
fromVarid _ = mzero

fromConid (Conid v) = return v
fromConid _ = mzero

fromDouble (Double v) = return v
fromDouble _ = mzero

fromInt (Integer v) = return v
fromInt _ = mzero

fromStr (String v) = return v
fromStr _ = mzero

parseId p = do
    n <- parseName p
    case n of
        Name [] ident -> return ident
        _ -> mzero
parseName f = do
    n <- next
    r <- f (extractLexeme n)
    return (fromText (extractLocation n) r)
parseLiteral f = do
    n <- next
    f (extractLexeme n)

varsym = parseId fromVarsym
qvarsym = parseName fromVarsym

varid = parseId fromVarid
qvarid = parseName fromVarid

conid = parseId fromConid
qconid = parseName fromConid
modid = qconid

float = parseLiteral fromDouble
integer = parseLiteral fromInt
string = parseLiteral fromStr

literal = fmap (Numeral . fromIntegral) integer <|> fmap Numeral float <|> fmap Text string
