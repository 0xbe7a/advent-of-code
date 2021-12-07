module Aoc.Y2021.D07
  ( day
  ) where

import           Control.Monad

import           Aoc.Day
import           Aoc.Parse
import Data.List (sort)

parser :: Parser [Integer]
parser = decimal `sepBy1` char ','

l1Metric :: Integer -> [Integer] ->  Integer
l1Metric t = sum . map (\p -> abs (p - t))

triDistance :: Integer -> [Integer] ->  Integer
triDistance t = sum . map (\p -> (abs (p - t) * (abs (p - t) + 1)) `div` 2 )

solver :: [Integer ] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ minimum $ map (`l1Metric` values) values
  putStrLn ">> Part 2"
  print $ minimum $ map (`triDistance` values) [minimum values..maximum values]

day :: Day
day = dayParse parser solver
