{-# LANGUAGE TupleSections #-}

module Aoc.Y2021.D15
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Applicative (Applicative (liftA2))
import Control.Arrow (Arrow (first, second))
import Control.Monad
import Data.Foldable (foldl')
import Data.HashPSQ (HashPSQ, alter, fromList, lookup, minView, size, unsafeInsertIncreasePriority, empty)
import Data.Maybe (fromJust, fromMaybe, isNothing, mapMaybe)
import qualified Debug.Trace as Debug
import GHC.Arr (Ix (inRange, range))

type Point = (Int, Int)
type Cave = [(Point, Integer)]

parser :: Parser (Point, Cave)
parser = do
  arrGrid <- some digit `sepEndBy1` newline
  let size = liftA2 (,) length (length . head) arrGrid
  let grid = zip (range ((1, 1), size)) $ concat arrGrid

  return (size, grid)

neighbors :: Point -> [Point]
neighbors (y, x) = [(y + dy, x + dx) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]

dijkstra :: Cave -> Point -> Point -> Integer
dijkstra cave start = dijkstra' frontier
  where
    -- This results in more unique prioritys
    totalWeight = sum $ map snd cave
    frontier = foldl' (\q (c, v, r) -> unsafeInsertIncreasePriority c v r q) Data.HashPSQ.empty $ zipWith toFrontier [totalWeight..] cave

    toFrontier i (c, r)
      | c == start = (c, 0, 0)
      | otherwise = (c, i, r)

dijkstra' :: HashPSQ Point Integer Integer -> Point -> Integer
dijkstra' frontier end = result
  where
    (curCoords, curDistance, risk, frontier') = fromJust $ minView frontier
    result
      | curCoords == end = curDistance
      | otherwise = dijkstra' frontier'' end

    -- Replace if present will update the distance if the point is part of the heap
    replaceIfPresent queue (coord, pv) = snd $ alter (minReplace pv) coord queue
    minReplace (newDistance, _) = (0,) . fmap (first (min newDistance))
    
    frontier'' = foldl' replaceIfPresent frontier' weightedNeighbors

    newDistance = liftA2 first (const . (+) curDistance . snd) id
    weightedNeighbors = mapMaybe (\c -> (c,) . newDistance <$> Data.HashPSQ.lookup c frontier') $ neighbors curCoords

expandGrid :: (Point, Cave) -> (Point, Cave)
expandGrid ((y, x), grid) = ((y', x'), grid')
  where
    inc = map (second (\w -> if w == 9 then 1 else w + 1))

    shiftRight = map (first (second (+ x)))
    shiftDown = map (first (first (+ y)))

    expandedRow = concat $ take 5 $ iterate (inc . shiftRight) grid
    grid' = concat $ take 5 $ iterate (inc . shiftDown) expandedRow
    (y', x') = (y * 5, x * 5)

solver :: (Point, Cave) -> IO ()
solver (size, grid) = do
  putStrLn ">> Part 1"
  print $ dijkstra grid (1, 1) size
  putStrLn ">> Part 2"
  let (expandedSize, expandedGrid) = expandGrid (size, grid)
  print $ dijkstra expandedGrid (1, 1) expandedSize

day :: Day
day = dayParse parser solver