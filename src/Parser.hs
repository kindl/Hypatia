{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Syntax
import qualified Data.Text.IO as Text
import Data.List(uncons)
import Data.Maybe(fromMaybe)
import Data.Functor(($>))
import Control.Applicative((<|>), optional, empty)
import Control.Monad(guard, (<$!>))
import Control.Monad.Trans.State.Strict(StateT(..), runStateT)
import Lexer(Lexeme(..), lexlex, prettyLocated,
    extractLexeme, extractLocation)
import Data.Attoparsec.Combinator(sepBy', sepBy1', many', many1')


{-
This module turns a list of lexemes into a syntax tree
-}
parse path s = do
    lexemes <- lexlex path s
    case runStateT modDecl lexemes of
        Nothing -> Left "Parse error at the beginning"
        Just (result, []) -> return result
        Just (_, rest:_) ->
            Left ("Could not parse until end. Next lexeme is: "
                ++ prettyLocated rest)

parseFile path = do
    str <- Text.readFile path
    fromEitherM (parse path str)

parseString s = parse "" s

-- Handles left recursion
-- for example in application fexpr
-- Previously used (foldl1' f <$!> many1' p)
-- but this created garbage in form of an intermediate list
chainl1' p op = p >>= rest
    where rest x = do f <- op
                      y <- p
                      rest $! f x y
                   <|> return x


-- get the next lexeme
next = StateT uncons
{-# INLINE next #-}

nextLexeme = extractLexeme <$!> next
{-# INLINE nextLexeme #-}

token text = do
    Reserved s <- nextLexeme
    guard (text == s)

-- parens combinators for readability
parenthesized p = token "(" *> p <* token ")"

curlyBraces p = token "{" *> p <* token "}"

bracketed p = token "[" *> p <* token "]"


{- Module -}
modDecl = do
    name <- optional (token "module" *> modid <* token ";")
    b <- body
    return (ModuleDeclaration (fromMaybe (fromText "Scratch") name) b)

{- Declarations -}
body = do
    is <- many' (impdecl <* token ";")
    ts <- many' (topdecl <* token ";")
    return (is ++ ts)

impdecl = do
    token "import"
    name <- modid
    imps <- optional impspec
    asAlias <- optional (token "as" *> modid)
    return (ImportDeclaration name imps asAlias)

impspec = parenthesized (sepBy' spec (token ","))

topdecl = typeDeclaration <|> aliasDeclaration <|> decl
typeDeclaration = do
    token "type"
    name <- con
    variables <- many' var
    constructors <- optional (token "=" *> constrs)
    return (TypeDeclaration name variables (concat constructors))
aliasDeclaration = do
    token "alias"
    alias <- conid
    token "="
    val <- otype
    return (AliasDeclaration alias val)
    
decls = curlyBraces (sepBy' decl (token ";"))
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
    i <- float
    o <- varsym
    a <- al
    return (FixityDeclaration f i o a)
functionDeclaration = do
    v <- var
    ps <- many1' apat
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
    ids <- many1' varid
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

btype = chainl1' atype (return TypeApplication)

atype = typeConstructor <|> typeVariable <|> parenthesizedType
typeConstructor = TypeConstructor <$!> qcon
typeVariable = TypeVariable <$!> var
parenthesizedType = ParenthesizedType <$!> parenthesized qtype

constrs = sepBy1' constr (token "|")
constr = do
    c <- con
    ts <- many' atype
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
prefixNegation = do
    Varsym [] "-" <- nextLexeme
    e <- infixexpr
    return (PrefixNegation e)
 
lexpr = lambdaExpression <|> letExpression
    <|> ifExpression <|> caseExpression <|> fexpr
lambdaExpression = do
    token "fun"
    ps <- many1' apat
    token "->"
    e <- expr
    return (LambdaExpression ps e)
letExpression = do
    token "let"
    ds <- decls
    token "in"
    e <- expr
    return (LetExpression ds e)
-- NOTE optional semicolons were removed
-- because they were part of parsing if- in do-expressions
ifExpression = do
    token "if"
    c <- expr
    token "then"
    t <- expr
    token "else"
    e <- expr
    return (IfExpression c t e)
caseExpression = do
    token "case"
    e <- expr
    token "of"
    als <- alts
    return (CaseExpression e als)

fexpr = chainl1' aexpr (return FunctionApplication)
 
aexpr = variable <|> constructorExpression <|> literalExpression
    <|> parenthesizedExpression <|> listExpression
variable = Variable <$!> qvar
constructorExpression = ConstructorExpression <$!> qcon
literalExpression = LiteralExpression <$!> literal
parenthesizedExpression =
    ParenthesizedExpression <$!> parenthesized expr
listExpression =
    ArrayExpression <$!> bracketed (sepBy' expr (token ","))

alts = curlyBraces (sepBy' alt (token ";"))
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
 
-- NOTE negative patterns were removed from lpat
-- because the minus is part of the literal
lpat = appliedConstructorPattern <|> apat
appliedConstructorPattern = do
    c <- qcon
    ps <- many1' apat
    return (ConstructorPattern c ps)

apat = wildcard <|> variablePattern
    <|> constructorPattern <|> literalPattern
    <|> parenthesizedPattern <|> arrayPattern
{-
TODO this should be apat and then var but it is left recursive
asPattern = do
    v <- var
    token "as"
    p <- apat
    return (AliasPattern v p)
-}
variablePattern = VariablePattern <$!> var
constructorPattern = do
    c <- qcon
    return (ConstructorPattern c [])
literalPattern = LiteralPattern <$!> literal
wildcard = token "_" $> Wildcard
parenthesizedPattern = ParenthesizedPattern <$!> parenthesized pat
arrayPattern = ArrayPattern <$!> bracketed (sepBy' pat (token ","))

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

fromVarsym (Varsym [] v) l = return (Id v l)
fromVarsym _ _ = Nothing

fromVarid (Varid [] v) l = return (Id v l)
fromVarid _ _ = Nothing

fromConid (Conid [] v) l = return (Id v l)
fromConid _ _ = Nothing

fromQVarsym (Varsym qs v) l = return (Name qs (Id v l))
fromQVarsym _ _ = Nothing

fromQVarid (Varid qs v) l = return (Name qs (Id v l))
fromQVarid _ _ = Nothing

fromQConid (Conid qs v) l = return (Name qs (Id v l))
fromQConid _ _ = Nothing

parseLocated f = do
    n <- next
    maybe empty return (f (extractLexeme n) (extractLocation n))

varsym = parseLocated fromVarsym
qvarsym = parseLocated fromQVarsym

varid = parseLocated fromVarid
qvarid = parseLocated fromQVarid

conid = parseLocated fromConid
qconid = parseLocated fromQConid
modid = qconid

float = do
    Double d <- nextLexeme
    return d
integer = do
    Integer i <- nextLexeme
    return i
string = do
    String s <- nextLexeme
    return s

literal =
    Numeral . fromIntegral <$!> integer
    <|> Numeral <$!> float
    <|> Text <$!> string
