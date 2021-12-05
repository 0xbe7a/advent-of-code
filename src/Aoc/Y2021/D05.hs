{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.D05
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Applicative (liftA2)
import Control.Monad
import Data.List (sortBy)
import qualified Data.Map as M (Map, empty, filter, insertWith, keys)

type Point = (Integer, Integer)

data Line = Line Point Point deriving (Eq, Show)

parser :: Parser [Line]
parser = manyLines $ Line <$> point <* string " -> " <*> point
  where
    point = (,) <$> decimal <* char ',' <*> decimal

findStraighLines = filter isStraight
  where
    isStraight (Line (x1, y1) (x2, y2))
      | x1 == x2 = True
      | y1 == y2 = True
      | otherwise = False

toPoints :: Line -> [Point]
toPoints (Line a b) = if length > 0 then traverseLine m c x2 x1 else map flip $ toPoints (Line (flip a) (flip b))
  where
    [(x1, y1), (x2, y2)] = sortBy (\a b -> fst a `compare` fst b) [a, b]
    length = x2 - x1
    m = (y2 - y1) `div` length
    c = (y1 * x2 - y2 * x1) `div` length
    flip (a, b) = (b, a)
    traverseLine m c ex x = (x, x * m + c) : if x < ex then traverseLine m c ex (x + 1) else []

counts :: Ord a => [a] -> M.Map a Integer
counts = foldr (\x m -> M.insertWith (+) x 1 m) M.empty

getIntersections :: M.Map a Integer -> [a]
getIntersections = M.keys . M.filter (>= 2)

solver :: [Line] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ length $ getIntersections . counts $ concatMap toPoints $ findStraighLines values

  putStrLn ">> Part 2"
  print $ length $ getIntersections . counts $ concatMap toPoints values

day :: Day
day = dayParse parser solver
