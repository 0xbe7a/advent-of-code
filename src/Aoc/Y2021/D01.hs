module Aoc.Y2021.D01
  ( day
  ) where

import           Control.Monad

import           Aoc.Day
import           Aoc.Parse

parser :: Parser [Integer]
parser = manyLines decimal

countIncreased :: Ord a => [a] -> Int
countIncreased values =  sum $ map fromEnum $ zipWith (<) values (tail values)

sliding :: Int -> [a] -> [[a]]
sliding n xs = if length xs < n then [] else take n xs : sliding n (tail xs)

solver :: [Integer] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ countIncreased values
  putStrLn ">> Part 2"
  print $ countIncreased $ map sum $ sliding 3 values

day :: Day
day = dayParse parser solver
