module Lexer where

import Prelude hiding (takeWhile)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text(Text)
import Control.Applicative((<|>), many, liftA2)
import Data.Char(isSpace, isLower, isUpper, isAlphaNum)
import Data.Traversable(mapAccumL)
import Syntax
import Data.Attoparsec.Text(char, satisfy, takeWhile, takeWhile1,
    sepBy1, match, parse, endOfInput, parseOnly, hexadecimal, double)

{-
This module converts text to a list of lexemes
Inspired by the Haskell2010 report
-}
lexlex path s =
    fmap pipeline (parseOnly (program path) s)

lexFileDebug2 path = do
    str <- Text.readFile path
    return (fmap (fmap extractLexeme) (lexlex path str))

lexFileDebug path = do
    str <- Text.readFile path
    return (parse (program path) str)

lexDebug str = parseOnly (program "") str

{- Layout -}
layout ls@(LocatedLexeme (Indent n) pos:ts) (m:ms)
    | n == m = semi pos : layout ts (m:ms)
    | n < m = close pos : layout ls ms
    | otherwise = layout ts (m:ms)
layout (LocatedLexeme (Block n) pos:ts) (m:ms)
    | n > m = open pos : layout ts (n : m : ms)
layout (LocatedLexeme (Block n) pos:ts) []
    | n > 0 = open pos : layout ts [n]
-- NOTE Removed. Allows only explicit empty blocks
--layout (LocatedLexeme (Block n) pos:ts) ms =
--    open pos : close pos :
--        layout (LocatedLexeme (Indent n) pos:ts) ms
layout (t@(LocatedLexeme (Reserved l) _):ts) (0:ms)
    | l == Text.pack "}" = t : layout ts ms
layout (t@(LocatedLexeme (Reserved l) _):ts) ms
    | l == Text.pack "}" = error ("Layout: Explicit "
        ++ prettyLocated t ++ " without open brace.")
layout (t@(LocatedLexeme (Reserved l) _):ts) ms
    | l == Text.pack "{" = t : layout ts (0:ms)
-- NOTE Parser rule left out.
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
--    | isWhite l && not (isSignificantWhite l) =
--        insertIndentTokens rest
insertIndentTokens (l:rest@(r:rs))
    | isLayout l && followedByOpen rest = l:insertIndentTokens rest
    | isLayout l = l:getBlock r:insertIndentTokens rs
    | isSignificantWhite l && isSignificantWhite r =
        insertIndentTokens rest
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

{- Locations -}
advance (Position l _) '\n' =
    Position (l + 1) 1
advance (Position l c) _ =
    Position l (c + 1)

initialPosition = Position 1 1

oneOf xs = satisfy (`elem` xs)

data LocatedLexeme = LocatedLexeme Lexeme Location
    deriving (Show)

extractLocation (LocatedLexeme _ p) = p
extractLexeme (LocatedLexeme l _) = l

prettyLocated (LocatedLexeme l p) = show l ++ " " ++ pretty p

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

{-
Attoparsec does not have location information,
but match returns the input that was consumed by a parser
the position is calculated from the input and passed to the next parser
-}
located path lexemes = snd (mapAccumL (\startPosition (parsed, result) ->
    let
        endPosition = Text.foldl' advance startPosition parsed
        location = Location startPosition endPosition path
        locatedLexeme = LocatedLexeme result location
    in (endPosition, locatedLexeme)) initialPosition lexemes)

program path = fmap (located path)
    (many (match (lexeme <|> whitespace)) <* endOfInput)

lexeme = literal <|> special <|> qvarid <|> qvarsym <|> qconid
special = do
    c <- oneOf "(),;[]`{}"
    return (Reserved (Text.pack [c]))

-- TODO multi-line comments
whitespace = whitechars <|> comment
whitechars = fmap Whitespace (takeWhile1 isSpace)
-- NOTE in contrast to the report this does not consume a newline
comment = fmap Comment (char '#' *> takeWhile (/='\n'))

-- parse a qualified p
optionalQualified parser =
    liftA2 mergeQualified (modid <* char '.') parser <|> parser

modid = do
    ms <- sepBy1 conid (char '.')
    return (foldr1 mergeQualified ms)

qconid = modid
qvarid = optionalQualified varid
qvarsym = optionalQualified varsym

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
literal = number <|> hexdecimal <|> verbatim

hexdecimal =
    char '0' *> (char 'x' <|> char 'X') *> fmap Integer hexadecimal

-- NOTE attoparsec parses a decimal as a double
-- for example 3 is parsed as 3.0
-- this solution here unfortunately parses 3.0 as 3
number = do
    d <- double
    return (case properFraction d of
        (i, 0.0) -> Integer i
        _ -> Double d)

verbatim = fmap (String . Text.pack)
    (char '"' *> many (stringChar <|> escapeSeq) <* char '"')

stringChar = satisfy (\x -> x /='"' && x /= '\\')
escapeSeq = char '\\' *> (char '\\' <|> char '"')
