module Aoc.Parse
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Char.Lexer
  , Parser
  , parseAndSolve
  , around
  , manyLines
  , lineWhile
  , lineUntil
  , lineSatisfy
  , line
  , lineChar
  , lineSpace
  , word
  , digit
  ) where

import           Data.Char
import           Data.Void

import qualified Data.Text                  as T
import           Text.Megaparsec            hiding (InvalidPosException, Pos,
                                             PosState, SourcePos, State,
                                             defaultTabWidth, initialPos, mkPos,
                                             pos1, sourcePosPretty, unPos)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (binary, decimal, float,
                                             hexadecimal, octal, scientific,
                                             signed)

-- The parser and applying it

type Parser = Parsec Void T.Text

parseAndSolve :: FilePath -> T.Text -> Parser a -> (a -> IO ()) -> IO ()
parseAndSolve path text parser solver = case parse (parser <* eof) path text of
  Right a -> solver a
  Left e  -> putStrLn $ errorBundlePretty e

-- General combinators

-- | Like 'between', but keeps the outer results instead of the inner result
around :: Applicative m => m i -> m l -> m r -> m (l, r)
around inner left right = (,) <$> (left <* inner) <*> right

-- AoC-specific parsers

manyLines :: Parser a -> Parser [a]
manyLines p = endBy (try p) newline

onLine :: (Char -> Bool) -> Char -> Bool
onLine _ '\n' = False
onLine p c    = p c

lineWhile :: (Char -> Bool) -> Parser T.Text
lineWhile = takeWhileP Nothing . onLine

lineUntil :: (Char -> Bool) -> Parser T.Text
lineUntil p = lineWhile (not . p)

lineSatisfy :: (Char -> Bool) -> Parser Char
lineSatisfy = satisfy . onLine

line :: Parser T.Text
line = lineWhile (const True)

lineChar :: Parser Char
lineChar = lineSatisfy (const True)

lineSpace :: Parser T.Text
lineSpace = lineWhile isSpace

word :: Parser T.Text
word = takeWhileP (Just "alphanumeric character") isAlphaNum

digit :: Num a => Parser a
digit = foldr1 (<|>)
  [ 0 <$ char '0'
  , 1 <$ char '1'
  , 2 <$ char '2'
  , 3 <$ char '3'
  , 4 <$ char '4'
  , 5 <$ char '5'
  , 6 <$ char '6'
  , 7 <$ char '7'
  , 8 <$ char '8'
  , 9 <$ char '9'
  ]
