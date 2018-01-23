module Lexer where

import Prelude hiding (takeWhile)
import qualified Data.Text as Text
import Data.Text(Text)
import Control.Applicative((<|>), many, some, optional, liftA2)
import Control.Monad(guard)
import Data.Char(isSpace, isLower, isUpper, isAlphaNum, isDigit, isHexDigit)
import Syntax
import Control.Monad.Trans.State(StateT(..), runStateT, get)

{-
This module converts a string to a list of lexemes
It was built with parsers like attoparsec in mind
but for now it only uses StateT to have position information
The state contains the rest of the stream and the current position
-}
data LexerState = LexerState Text Position
    deriving (Show)

advance (Position l _) '\n' =
    Position (l + 1) 0
advance (Position l c) _ =
    Position l (c + 1)

startState s = LexerState (Text.pack s) (Position 1 0)

-- get the next token from the stream and advance the position
satisfyState p (LexerState t position) = do
    (c, cs) <- Text.uncons t
    guard (p c)
    return (c, LexerState cs (advance position c))
satisfy p = StateT (satisfyState p)

char a = satisfy (== a)

oneOf xs = satisfy (`elem` xs)

takeWhile p = fmap Text.pack (many (satisfy p))
takeWhile1 p = fmap Text.pack (some (satisfy p))

sepBy parser seperator =
    fmap concat (optional (sepBy1 parser seperator))
sepBy1 parser seperator =
    liftA2 (:) parser (many (seperator *> parser))

lexlex path s =
    fmap pipeline (eitherResult (runStateT (program path) (startState s)))

eitherResult Nothing = throwString "Incomplete lex"
eitherResult (Just (r, LexerState t pos)) =
    if Text.null t then return r
        else throwString ("Incomplete lex at " ++ pretty pos)

lexFileDebug2 path = do
    str <- readFile path
    return (fmap (fmap extractLexeme) (lexlex path str))

lexFileDebug path = do
    str <- readFile path
    return (runStateT (program path) (startState str))

lexDebug str = runStateT (program "") (startState str)


{-
Layout
A simplified version of the function presented in the Haskell 2010 report
-}
layout ls@(LocatedLexeme (Indent n) pos:ts) (m:ms)
    | n == m = semi pos : layout ts (m:ms)
    | n < m = close pos : layout ls ms
    | otherwise = layout ts (m:ms)
layout (LocatedLexeme (Block n) pos:ts) (m:ms)
    | n > m = open pos : layout ts (n : m : ms)
layout (LocatedLexeme (Block n) pos:ts) []
    | n > 0 = open pos : layout ts [n]
layout (LocatedLexeme (Block n) pos:ts) ms =
    open pos : close pos : layout (LocatedLexeme (Indent n) pos:ts) ms
layout (t@(LocatedLexeme (Reserved l) _):ts) (0:ms)
    | l == Text.pack "}" = t : layout ts ms
layout (t@(LocatedLexeme (Reserved l) _):ts) ms
    | l == Text.pack "}" =
        error ("Parse Error: Explicit " ++ prettyLocated t ++ " without open brace.")
layout (t@(LocatedLexeme (Reserved l) _):ts) ms
    | l == Text.pack "{" = t : layout ts (0:ms)
-- rule left out: no info from parser
layout (t:ts) ms = t:layout ts ms
layout [] [] = []
layout [] (m:ms) = close builtinLocation : layout [] ms

semi pos = LocatedLexeme (Reserved (Text.pack ";")) pos
open pos = LocatedLexeme (Reserved (Text.pack "{")) pos
close pos = LocatedLexeme (Reserved (Text.pack "}")) pos

isLayout (LocatedLexeme (Reserved t) _)
    | elem t (fmap Text.pack ["let", "where", "of"]) = True
isLayout _ = False

-- + 1 because it is the column
indLength ws = Text.length (last (Text.split (=='\n') ws)) + 1

getBlock (LocatedLexeme (Whitespace ws) pos) =
    LocatedLexeme (Block (indLength ws)) pos

getIndent (LocatedLexeme (Whitespace ws) pos) =
    LocatedLexeme (Indent (indLength ws)) pos

followedByOpen rest = case dropWhile isWhite rest of
    (LocatedLexeme (Reserved t) _:_) | t == Text.pack "{" -> True
    _ -> False

-- TODO find the logic error in here
--insertIndentTokens (l:rest)
--    | isWhite l && not (isSignificantWhite l)= insertIndentTokens rest
insertIndentTokens (l:rest@(r:rs))
    | isLayout l && followedByOpen rest = l:insertIndentTokens rest
    | isLayout l = l:getBlock r:insertIndentTokens rs
    | isSignificantWhite l && isSignificantWhite r = insertIndentTokens rest
    | isSignificantWhite l && isWhite r = insertIndentTokens (l:rs)
insertIndentTokens (l:rest)
    | isSignificantWhite l = getIndent l:insertIndentTokens rest
    | isWhite l = insertIndentTokens rest
    | otherwise = l:insertIndentTokens rest
insertIndentTokens [] = []

-- Cut the open and close braces
cut xs = init (tail xs)

pipeline ls = cut (layout (
    LocatedLexeme (Block 1) builtinLocation : insertIndentTokens ls) [])

isWhite (LocatedLexeme (Whitespace _) _) = True
isWhite (LocatedLexeme (Comment _) _) = True
isWhite _ = False

isSignificantWhite (LocatedLexeme (Whitespace ws) _) =
    Text.any (=='\n') ws
isSignificantWhite _ = False


data LocatedLexeme = LocatedLexeme Lexeme Location
    deriving (Show)

extractLocation (LocatedLexeme _ p) = p
extractLexeme (LocatedLexeme l _) = l

prettyLocated (LocatedLexeme l p) = show l ++ " " ++ pretty p

extractText (Conid s) = s
extractText (Varid s) = s
extractText (Varsym s) = s
extractText _ = error "extractText"

data Lexeme
    = Reserved Text
    | Whitespace Text
    | Double Double
    | Integer Integer
    | String Text
    | Varid Text
    | Varsym Text
    | Conid Text
    | Comment Text
    | Block Int
    | Indent Int
        deriving (Show)

dot c1 c2 = c1 `mappend` Text.pack "." `mappend` c2

mergeQualified (Conid c1) (Conid c2) =
    Conid (c1 `dot` c2)
mergeQualified (Conid c1) (Varid c2) =
    Varid (c1 `dot` c2)
mergeQualified (Conid c1) (Varsym c2) =
    Varsym (c1 `dot` c2)
mergeQualified _ _ = error "mergeQualified"

{- Lexer -}
located path l = do
    LexerState _ startPosition <- get
    result <- l
    LexerState _ endPosition <- get
    return (LocatedLexeme result (Location startPosition endPosition path))

program path = many (located path (lexeme <|> whitespace))
lexeme = literal <|> special <|> qvarid <|> qvarsym <|> qconid
special = do
    c <- oneOf "(),;[]`{}."
    return (Reserved (Text.pack [c]))

-- TODO: multi-line comments
whitespace = whitechars <|> comment
whitechars = fmap Whitespace (takeWhile1 isSpace)
comment = do
    char '#'
    cs <- takeWhile (/='\n')
    char '\n'
    return (Comment cs)

-- parse a qualified p
qualif parser = point parser <|> parser

point parser =
  do
    q <- modid
    char '.'
    res <- parser
    return (mergeQualified q res)

modid = do
    ms <- sepBy1 conid (char '.')
    return (Conid (mintercalate (Text.pack ".") (fmap extractText ms)))

qconid = modid
qvarid = qualif varid
qvarsym = qualif varsym

small = satisfy isLower <|> char '_'
large = satisfy isUpper

{- Identifier -}
isReserved x =
    elem x (fmap Text.pack ["alias", "enum", "type", "forall",
        "import", "module", "fun", "let", "in", "where", "case", "of",
            "if", "then", "else", "infix", "infixl", "infixr", "as"])
varid = do
    x <- small
    xs <- takeWhile isAlphaNum
    let id = Text.cons x xs
    return (if isReserved id then Reserved id else Varid id)
conid = do
    x <- large
    xs <- takeWhile isAlphaNum
    let id = Text.cons x xs
    return (Conid id)

{- Symbols -}
isReservedOp x = elem x (fmap Text.pack [":", "=", "->", "|"])
varsym = do
    s <- takeWhile1 isSym
    return (if isReservedOp s then Reserved s else Varsym s)

{- Literal -}
literal = fmap makeDouble float <|> fmap makeInteger decimal
    <|> fmap makeInteger hexadecimal <|> fmap String verbatim

makeDouble = Double . read . Text.unpack
makeInteger = Integer . read . Text.unpack

decimal = takeWhile1 isDigit
hexadecimal = do
    z <- char '0'
    x <- char 'x' <|> char 'X'
    s <- takeWhile1 isHexDigit
    return (Text.pack [z,x] `mappend` s)

float = decDotDecExpo <|> decDotDec <|> decExpo
decDotDec = liftA2 dot (decimal <* char '.') decimal
decDotDecExpo = liftA2 mappend decDotDec expo
decExpo = liftA2 mappend decimal expo
expo = do
    e <- char 'e' <|> char 'E'
    sign <- optional (char '+' <|> char '-')
    ds <- decimal
    return (Text.cons e (toText sign) `mappend` ds)

toText = foldr Text.cons mempty

verbatim = do
    char '"'
    cs <- many (stringChar <|> escapeSeq)
    char '"'
    return (Text.pack cs)

stringChar = satisfy (\x -> x /='"' && x /= '\\')
escapeSeq = char '\\' *> (char '\\' <|> char '"')
