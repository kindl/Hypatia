module Lexer where

import Prelude hiding (takeWhile)
import qualified Data.Text as Text
import Data.Text(Text)
import Control.Applicative((<|>), many, some, optional, liftA2)
import Control.Monad(guard)
import Data.Char(isSpace, isLower, isUpper, isAlphaNum, isDigit, isHexDigit)
import Data.List(groupBy)
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

lexFileDebug path = do
    str <- readFile path
    return (runStateT (program path) (startState str))

lexDebug str = runStateT (program "") (startState str)

pipeline = insertSemicolons . filter (not . null)
    . setIndentation . filter isSignificant

isSignificant x = isSignificantWhite x || not (isWhite x)

isWhite (LocatedLexeme (Whitespace _) _ _) = True
isWhite (LocatedLexeme (Comment _) _ _) = True
isWhite _ = False

-- TODO brace insertion

{- Semicolon insertion -}
insertSemicolons [] = []
-- Lines have to be non empty
insertSemicolons ls = foldl1 semiMerge ls

semiMerge [] _ = error "semiMerge impossible empty left"
semiMerge _ [] = error "semiMerge impossible empty right"
semiMerge line1 line2 =
  let
    prevLexeme = last line1
    nextLexeme = head line2

    Location start _ path = extractLocation prevLexeme
    Location _ end _ = extractLocation nextLexeme
    l = Reserved (Text.pack ";")
    loc = Location start end path

    ind1 = extractIndentation prevLexeme
    ind2 = extractIndentation nextLexeme

    le = LocatedLexeme l loc ind1
  in case extractLexeme nextLexeme of 
    Reserved r | r == Text.pack "}" -> line1 ++ line2
{-
TODO rework this hack allowing aligning enums e.g.
enum Vector a
    = Vec2 a a
    | Vec4 a a a a
-}
    Reserved r | r == Text.pack "|" -> line1 ++ line2
{-
TODO rework this hack allowing aligning Matrixes e.g.
m1 = Mat2
    (Vec2 1 2)
    (Vec2 3 4)
-}
    Reserved r | r == Text.pack "(" && ind2 > 0 -> line1 ++ line2
    _ -> case extractLexeme prevLexeme of
      Reserved r | r == Text.pack ";" -> line1 ++ line2
      _ -> if ind1 < ind2 then line1 ++ line2 else line1 ++ le:line2

{- Indentation -}
setIndentation ls =
    fmap setIndentLine (breaks isSignificantWhite ls)

-- breaks the lexemes into lines
breaks f = groupBy (const (not . f))

isSignificantWhite (LocatedLexeme (Whitespace ws) _ _) =
    Text.any (=='\n') ws
isSignificantWhite _ = False

-- set the indentation of a line, if it starts with whitespace
setIndentLine [] = error "setIndentLine impossible case"
setIndentLine (l:ls) =
    case extractLexeme l of
        Whitespace ws ->
            let ind = Text.length (last (Text.lines ws))
            in fmap (toIndentedLexeme ind) ls
        _ -> fmap (toIndentedLexeme 0) (l:ls)

toIndentedLexeme ind (LocatedLexeme l loc _) =
    LocatedLexeme l loc ind 

type Indentation = Int
data LocatedLexeme = LocatedLexeme Lexeme Location Indentation
    deriving (Show)

extractIndentation (LocatedLexeme _ _ i) = i
extractLocation (LocatedLexeme _ p _) = p
extractLexeme (LocatedLexeme l _ _) = l

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
    return (LocatedLexeme result (Location startPosition endPosition path) (-1))

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
    elem x (fmap Text.pack ["alias", "enum", "type", "forall", "import", "module", "fun", "let", "in", "where",
        "case", "of", "if", "then", "else", "infix", "infixl", "infixr", "as"])
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
