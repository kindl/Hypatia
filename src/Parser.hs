{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Syntax
import qualified Data.Text.IO as Text
import Data.List(uncons, foldl1')
import Data.Maybe(fromMaybe)
import Data.Functor(($>))
import Control.Applicative((<|>), optional, empty, liftA2)
import Control.Monad(guard, (<$!>))
import Control.Monad.Trans.State.Strict(StateT(..), runStateT)
import Lexer(Lexeme(..), lexlex, prettyLocated,
    extractLexeme, extractLocation)
import Data.Attoparsec.Combinator(sepBy', sepBy1', many', many1', eitherP)


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
{-# INLINE parse #-}

parseFile path = do
    str <- Text.readFile path
    either fail return (parse path str)
{-# INLINE parseFile #-}

parseString s = parse "" s
{-# INLINE parseString #-}

-- get the next lexeme
next = StateT uncons
{-# INLINE next #-}

nextLexeme = extractLexeme <$!> next
{-# INLINE nextLexeme #-}

token text = do
    Reserved s <- nextLexeme
    guard (text == s)
{-# INLINE token #-}

-- parens combinators for readability
parenthesized p = token "(" *> p <* token ")"
{-# INLINE parenthesized #-}

curlyBraces p = token "{" *> p <* token "}"
{-# INLINE curlyBraces #-}

bracketed p = token "[" *> p <* token "]"
{-# INLINE bracketed #-}


{- Module -}
modDecl = do
    name <- optional (token "module" *> modid <* token ";")
    b <- body
    return (ModuleDeclaration (fromMaybe (fromText "Scratch") name) b)
{-# INLINE modDecl #-}

{- Declarations -}
body = do
    is <- many' (impdecl <* token ";")
    ts <- many' (topdecl <* token ";")
    return (is ++ ts)
{-# INLINE body #-}

impdecl = do
    token "import"
    name <- modid
    imps <- optional impspec
    asAlias <- optional (token "as" *> modid)
    return (ImportDeclaration name imps asAlias)
{-# INLINE impdecl #-}

impspec = parenthesized (sepBy' spec (token ","))
{-# INLINE impspec #-}

topdecl = typeDeclaration <|> aliasDeclaration <|> decl
{-# INLINE topdecl #-}

typeDeclaration = do
    token "type"
    name <- con
    variables <- many' var
    constructors <- optional (token "=" *> constrs)
    return (TypeDeclaration (fromId name) variables (concat constructors))
{-# INLINE typeDeclaration #-}

aliasDeclaration = do
    token "alias"
    alias <- conid
    token "="
    val <- otype
    return (AliasDeclaration (fromId alias) val)
{-# INLINE aliasDeclaration #-}

decls = curlyBraces (sepBy' decl (token ";"))
{-# INLINE decls #-}

decl = typeSignature <|> fixityDeclaration
    <|> operatorDeclaration <|> functionDeclaration
    <|> expressionDeclaration
{-# INLINE decl #-}

typeSignature = do
    v <- var
    token ":"
    t <- qtype
    return (TypeSignature (fromId v) t)
{-# INLINE typeSignature #-}

fixityDeclaration = do
    f <- fixity
    i <- float
    o <- varsym
    a <- varid <|> conid
    return (FixityDeclaration f i (fromId o) (fromId a))
{-# INLINE fixityDeclaration #-}

functionDeclaration = do
    v <- var
    ps <- many1' apat
    r <- rhs
    return (FunctionDeclaration (fromId v) ps r)
{-# INLINE functionDeclaration #-}

operatorDeclaration = do
    p1 <- lpat
    o <- varsym
    p2 <- lpat
    r <- rhs
    return (FunctionDeclaration (fromId o) [p1, p2] r)
{-# INLINE operatorDeclaration #-}

expressionDeclaration = do
    p <- lpat
    token "="
    aw <- optional (token "await")
    e <- exprWithWhere
    return (ExpressionDeclaration p (case aw of
        Just _ -> AwaitExpression e
        Nothing -> e))
{-# INLINE expressionDeclaration #-}

rhs = token "=" *> exprWithWhere
{-# INLINE rhs #-}

exprWithWhere = do
    e <- expr
    ws <- optional (token "where" *> decls)
    return (maybe e (flip LetExpression e) ws)
{-# INLINE exprWithWhere #-}

fixity = token "infixl" $> LeftAssociative
    <|> token "infixr" $> RightAssociative
    <|> token "infix" $> None
{-# INLINE fixity #-}

{- Types -}
qtype = forAll <|> otype
{-# INLINE qtype #-}

forAll = do
    token "forall"
    ids <- many1' varid
    token "."
    t <- otype
    return (ForAll ids t)
{-# INLINE forAll #-}

{-
otype could be defined as
otype = typeArrow <|> typeOperator <|> btype
but because all options would start with a btype 
a lot of backtracking might be necessary
Better: first parse a btype and then decide
what to parse on the following token "->" or qvarsym
-}
otype = do
    b <- btype
    n <- optional (liftA2 (,) (eitherP (token "->") qvarsym) otype)
    return (case n of
        Just (Left _, t) -> TypeArrow b t
        Just (Right o, t) -> TypeInfixOperator b o t
        Nothing -> b)
{-# INLINE otype #-}

-- NOTE The left fold handles left recursion
btype = foldl1' TypeApplication <$!> many1' atype
{-# INLINE btype #-}

atype = typeConstructor <|> typeVariable <|> parenthesizedType
{-# INLINE atype #-}

typeConstructor = TypeConstructor <$!> qcon
{-# INLINE typeConstructor #-}

typeVariable = TypeVariable <$!> var
{-# INLINE typeVariable #-}

parenthesizedType = ParenthesizedType <$!> parenthesized qtype
{-# INLINE parenthesizedType #-}

constrs = sepBy1' constr (token "|")
{-# INLINE constrs #-}

constr = do
    c <- con
    ts <- many' atype
    return (fromId c, ts)
{-# INLINE constr #-}

{- Expressions -}
expr = {- typeAnnotation <|> -} infixexpr
{-# INLINE expr #-}

{-
-- in the same way as infixOperatorOrLexpr
typeAnnotationOrInfixExpr = do
    e <- infixexpr
    ot <- optional (token ":" *> qtype)
    return (case ot of
        Just t -> TypeAnnotation e t
        Nothing -> e)
{-# INLINE typeAnnotationOrInfixExpr #-}
-}

infixexpr = prefixNegation <|> infixOperatorOrLexpr
{-# INLINE infixexpr #-}

{-
the same as infixOperator <|> lexpr
because infixOperator would start with an lexpr
and fail if it is not an infixOperator
lexpr would be parsed twice
-}
infixOperatorOrLexpr = do
    l <- lexpr
    mo <- optional (liftA2 (,) qvarsym infixexpr)
    return (case mo of
        Just (o, r) -> InfixOperator l o r
        Nothing -> l)
{-# INLINE infixOperatorOrLexpr #-}

prefixNegation = do
    Varsym [] "-" <- nextLexeme
    e <- infixexpr
    return (PrefixNegation e)
{-# INLINE prefixNegation #-}

lexpr = lambdaExpression <|> letExpression
    <|> ifExpression <|> caseExpression <|> fexpr
{-# INLINE lexpr #-}

lambdaExpression = do
    token "fun"
    ps <- many1' apat
    token "->"
    e <- expr
    return (LambdaExpression ps e)
{-# INLINE lambdaExpression #-}

letExpression = do
    token "let"
    ds <- decls
    token "in"
    e <- expr
    return (LetExpression ds e)
{-# INLINE letExpression #-}

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
{-# INLINE ifExpression #-}

caseExpression = do
    token "case"
    e <- expr
    token "of"
    als <- alts
    return (CaseExpression e als)
{-# INLINE caseExpression #-}

-- NOTE The left fold handles left recursion
fexpr = foldl1' FunctionApplication <$!> many1' aexpr
{-# INLINE fexpr #-}

aexpr = variable <|> constructorExpression <|> literalExpression
    <|> parenthesizedExpression <|> listExpression
{-# INLINE aexpr #-}

variable = Variable <$!> qvar
{-# INLINE variable #-}

constructorExpression = ConstructorExpression <$!> qcon
{-# INLINE constructorExpression #-}

literalExpression = LiteralExpression <$!> literal
{-# INLINE literalExpression #-}

parenthesizedExpression =
    ParenthesizedExpression <$!> parenthesized expr
{-# INLINE parenthesizedExpression #-}

listExpression =
    ArrayExpression <$!> bracketed (sepBy' expr (token ","))
{-# INLINE listExpression #-}

alts = curlyBraces (sepBy' alt (token ";"))
{-# INLINE alts #-}

alt = do
    p <- pat
    token "->"
    e <- exprWithWhere
    return (p, e)
{-# INLINE alt #-}

{- Patterns -}
pat = patternInfixOperator <|> lpat
{-# INLINE pat #-}

patternInfixOperator = do
    l <- lpat
    o <- qvarsym
    r <- pat
    return (PatternInfixOperator l o r)
{-# INLINE patternInfixOperator #-}

-- NOTE negative patterns were removed from lpat
-- because the minus is part of the literal
lpat = appliedConstructorPattern <|> apat
{-# INLINE lpat #-}

appliedConstructorPattern = do
    c <- qcon
    ps <- many1' apat
    return (ConstructorPattern c ps)
{-# INLINE appliedConstructorPattern #-}

apat = aliasPattern <|> wildcard <|> variablePattern
    <|> constructorPattern <|> literalPattern
    <|> parenthesizedPattern <|> arrayPattern
{-# INLINE apat #-}

{- NOTE
aliasPattern uses a keyword instead of @, allows pat instead of apat
and requires enclosing parentheses e.g. (s alias Sphere p v c r)

Here are some alternative ideas
(Sphere p v c r as s)       hard to read because as looks like a variable
(s as Sphere p v c r)       would be clearer, but switched compared to imports
                            e.g. import Viewer.Obj as Obj

TODO warn or eliminate patterns of the form
(aVar alias anotherVar) and (aVar alias (anotherVar))
-}
aliasPattern = parenthesized (do
    v <- var
    token "alias"
    p <- pat
    return (AliasPattern (fromId v) p))
{-# INLINE aliasPattern #-}

variablePattern = (VariablePattern . fromId) <$!> var
{-# INLINE variablePattern #-}

constructorPattern = do
    c <- qcon
    return (ConstructorPattern c [])
{-# INLINE constructorPattern #-}
    
literalPattern = LiteralPattern <$!> literal
{-# INLINE literalPattern #-}

parenthesizedPattern = ParenthesizedPattern <$!> parenthesized pat
{-# INLINE parenthesizedPattern #-}

arrayPattern = ArrayPattern <$!> bracketed (sepBy' pat (token ","))
{-# INLINE arrayPattern #-}

{-
Read the following like this example:
A variable can be a varid like x
or a varsym like (+)
-}
spec = varid <|> conid <|> parenthesized varsym
{-# INLINE spec #-}

var = varid <|> parenthesized varsym
{-# INLINE var #-}

qvar = qvarid <|> parenthesized qvarsym
{-# INLINE qvar #-}

con = conid <|> parenthesized varsym
{-# INLINE con #-}

qcon = qconid <|> parenthesized qvarsym
{-# INLINE qcon #-}

{-
Read the following like this example:
A qvarid can be a qualified variable like List.map
or a normal variable identifier like map
-}

fromVarsym (Varsym [] v) l = return (Id v l)
fromVarsym _ _ = Nothing
{-# INLINE fromVarsym #-}

fromVarid (Varid [] v) l = return (Id v l)
fromVarid _ _ = Nothing
{-# INLINE fromVarid #-}

fromConid (Conid [] v) l = return (Id v l)
fromConid _ _ = Nothing
{-# INLINE fromConid #-}

fromQVarsym (Varsym qs v) l = return (Name qs (Id v l))
fromQVarsym _ _ = Nothing
{-# INLINE fromQVarsym #-}

fromQVarid (Varid qs v) l = return (Name qs (Id v l))
fromQVarid _ _ = Nothing
{-# INLINE fromQVarid #-}

fromQConid (Conid qs v) l = return (Name qs (Id v l))
fromQConid _ _ = Nothing
{-# INLINE fromQConid #-}

fromWildcard (Reserved "_") l = return (Wildcard (Id "_" l))
fromWildcard _ _ = Nothing
{-# INLINE fromWildcard #-}

parseLocated f = do
    n <- next
    maybe empty return (f (extractLexeme n) (extractLocation n))
{-# INLINE parseLocated #-}

varsym = parseLocated fromVarsym
{-# INLINE varsym #-}

qvarsym = parseLocated fromQVarsym
{-# INLINE qvarsym #-}

varid = parseLocated fromVarid
{-# INLINE varid #-}

qvarid = parseLocated fromQVarid
{-# INLINE qvarid #-}

conid = parseLocated fromConid
{-# INLINE conid #-}

qconid = parseLocated fromQConid
{-# INLINE qconid #-}

wildcard = parseLocated fromWildcard
{-# INLINE wildcard #-}

modid = qconid
{-# INLINE modid #-}

float = do
    Double d <- nextLexeme
    return d
{-# INLINE float #-}

integer = do
    Integer i <- nextLexeme
    return i
{-# INLINE integer #-}

string = do
    String s <- nextLexeme
    return s
{-# INLINE string #-}

literal =
    Numeral . intToDouble <$!> integer
    <|> Numeral <$!> float
    <|> Text <$!> string
{-# INLINE literal #-}
