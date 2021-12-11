module Aoc.Y2021.D10
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Text                  as T
import Data.List (sort)

parser :: Parser [T.Text]
parser = manyLines line

parseLine :: String -> String
parseLine = foldl parseFold []
    where
        parseFold ")" _ = ")"
        parseFold "]" _ = "]"
        parseFold "}" _ = "}"
        parseFold ">" _ = ">"

        parseFold s '(' = '(':s
        parseFold s '[' = '[':s
        parseFold s '{' = '{':s
        parseFold s '<' = '<':s

        parseFold ('(':s) ')' = s
        parseFold ('[':s) ']' = s
        parseFold ('{':s) '}' = s
        parseFold ('<':s) '>' = s

        parseFold _ c = [c]


score ")" = 3
score "]" = 57
score "}" = 1197
score ">" = 25137
score _ = 0

score2 = foldl scoreFold 0
    where
        scoreFold acc '(' = 1 + acc * 5
        scoreFold acc '[' = 2 + acc * 5
        scoreFold acc '{' = 3 + acc * 5
        scoreFold acc '<' = 4 + acc * 5
        scoreFold _ x = 0

middle list = list !! (length list `div` 2)

solver :: [T.Text] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ sum $ map (score . parseLine . T.unpack) values
  putStrLn ">> Part 2"
  print $ middle $ sort $ filter (>0) $ map (score2 . parseLine . T.unpack) values

day :: Day
day = dayParse parser solver
