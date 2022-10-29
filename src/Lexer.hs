{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import Prelude hiding (takeWhile)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text(Text)
import Prettyprinter(pretty)
import Control.Applicative((<|>))
import Data.Functor(($>))
import Data.Char(isSpace, isUpper, isAlphaNum)
import Data.Traversable(mapAccumL)
import Syntax
-- NOTE strict combinators are used
-- however <$!> instead of fmap would decrease performance
import Data.Attoparsec.Text(char, satisfy, takeWhile, takeWhile1,
    many', sepBy1', match, parse, endOfInput, parseOnly,
    hexadecimal, double)

{-
This module converts text to a list of lexemes
Inspired by the Haskell2010 report
-}
lexlex path s =
    fmap withIndentTokens (parseOnly (program path) s)
{-# INLINE lexlex #-}

lexFileDebug path =
    fmap (parse (program path)) (Text.readFile path)

lexFileDebug2 path =
    fmap (fmap (fmap extractLexeme) . lexlex path)
        (Text.readFile path)

lexDebug = parseOnly (program "")

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
    | l == "}" = t : layout ts ms
layout (t@(LocatedLexeme (Reserved l) _):_) _
    | l == "}" = error ("Layout: Explicit "
        ++ prettyLocated t ++ " without open brace.")
layout (t@(LocatedLexeme (Reserved l) _):ts) ms
    | l == "{" = t : layout ts (0:ms)
-- NOTE Parser rule left out.
layout (t:ts) ms = t:layout ts ms
layout [] [] = []
layout [] (_:ms) = close builtinLocation : layout [] ms
{-# INLINE layout #-}

semi pos = LocatedLexeme (Reserved ";") pos
open pos = LocatedLexeme (Reserved "{") pos
close pos = LocatedLexeme (Reserved "}") pos

isLayout (LocatedLexeme (Reserved t) _)
    | elem t ["let", "where", "of"] = True
isLayout _ = False

-- add 1 because counting columns starts from 1
indLength ws = Text.length (last (Text.split (=='\n') ws)) + 1

getBlock (LocatedLexeme (Whitespace ws) pos) =
    LocatedLexeme (Block (indLength ws)) pos
getBlock l =
    error ("getBlock impossible case at " ++ prettyLocated l)

getIndent (LocatedLexeme (Whitespace ws) pos) =
    LocatedLexeme (Indent (indLength ws)) pos
getIndent l =
    error ("getIndent impossible case at " ++ prettyLocated l)


followedByOpen rest = case dropWhile isWhite rest of
    (LocatedLexeme (Reserved t) _:_) | t == "{" -> True
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
{-# INLINE insertIndentTokens #-}

-- Cut the open and close braces
cut xs = init (tail xs)

withIndentTokens ls = cut (layout (
    LocatedLexeme (Block 1) builtinLocation : insertIndentTokens ls) [])
{-# INLINE withIndentTokens #-}

isWhite (LocatedLexeme (Whitespace _) _) = True
isWhite (LocatedLexeme (Comment _) _) = True
isWhite _ = False

isSignificantWhite (LocatedLexeme (Whitespace ws) _) =
    Text.elem '\n' ws
isSignificantWhite _ = False

{- Locations -}
advance (Position l _) '\n' =
    Position (l + 1) 1
advance (Position l c) _ =
    Position l (c + 1)

initialPosition = Position 1 1

oneOf xs = satisfy (flip Text.elem xs)
{-# INLINE oneOf #-}

data LocatedLexeme = LocatedLexeme Lexeme Location
    deriving (Show)

extractLocation (LocatedLexeme _ p) = p
extractLexeme (LocatedLexeme l _) = l

prettyLocated (LocatedLexeme l p) =
    show l ++ " " ++ show (pretty p)

data Lexeme
    = Reserved Text
    | Whitespace Text
    | Double Double
    | Integer Int
    | String Text
    | Varid [Text] Text
    | Varsym [Text] Text
    | Conid [Text] Text
    | Comment Text
    | Block Int
    | Indent Int
        deriving (Show)

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
{-# INLINE located #-}

program path = fmap (located path)
    (many' (match (whitespace <|> lexeme)) <* endOfInput)

lexeme = literal <|> special <|> qident
{-# INLINE lexeme #-}

-- NOTE . was addded
-- it is part of for example forall a. a
special = fmap (Reserved . Text.singleton) (oneOf "(),;[]`{}.")
{-# INLINE special #-}

-- TODO multi-line comments
whitespace = whitechars <|> hashcomment <|> slashcomment
{-# INLINE whitespace #-}

whitechars = fmap Whitespace (takeWhile1 isSpace)
{-# INLINE whitechars #-}

-- NOTE in contrast to the report this does not consume a newline
hashcomment = fmap Comment (char '#' *> takeWhile (/='\n'))
{-# INLINE hashcomment #-}

slashcomment = fmap Comment (char '/' *> char '/' *> takeWhile (/='\n'))
{-# INLINE slashcomment #-}

qident = fmap (\ms -> makeIdent (init ms) (last ms))
    (sepBy1' (ident <|> varsym) (char '.'))
{-# INLINE qident #-}

-- TODO improve this check
-- qualifiers have to start with an uppercase qualifier
makeIdent ms i | any (firstIs (not . isUpper)) ms =
    error ("Bad qualifier for identifier " ++ show i)
makeIdent [] i | elem i reserved = Reserved i
makeIdent ms i | firstIs isUpper i = Conid ms i
makeIdent ms i | firstIs isSym i = Varsym ms i
makeIdent ms i = Varid ms i
{-# INLINE makeIdent #-}

{- Identifiers -}
reserved = ["alias", "enum", "type", "forall", "await",
        "import", "module", "fun", "let", "in", "where", "case", "of",
        "if", "then", "else", "infix", "infixl", "infixr", "as", "_",
        ":", "=", "->", "|", "\\"]

ident = takeWhile1 (\x -> isAlphaNum x || x == '_')
{-# INLINE ident #-}

varsym = takeWhile1 isSym
{-# INLINE varsym #-}

{- Literal -}
literal = number <|> hexdecimal <|> verbatim
{-# INLINE literal #-}

hexdecimal = fmap Integer
    (char '0' *> oneOf "xX" *> hexadecimal)
{-# INLINE hexdecimal #-}

-- TODO parse integers
-- attoparsec parses a decimal as a double
-- for example 3 is parsed as 3.0
number = fmap Double double
{-# INLINE number #-}

verbatim = fmap (String . Text.pack)
    (char '"' *> many' (stringChar <|> escapeSeq) <* char '"')
{-# INLINE verbatim #-}

stringChar = satisfy (\x -> x /='"' && x /= '\\')
{-# INLINE stringChar #-}

escapeSeq = char '\\' *> (char '\\' <|> char '\"' <|>
    char 'n' $> '\n' <|> char 'r' $> '\r' <|> char 't' $> '\t')
{-# INLINE escapeSeq #-}
