module Aoc.Y2021.D09
  ( day
  ) where

import           Control.Monad

import           Aoc.Day
import           Aoc.Parse
import GHC.Arr (Array, listArray, (!), bounds, indices, elems, assocs)
import Control.Applicative (Applicative(liftA2))
import Data.Ix
import Debug.Trace
import Data.List (sortBy, sort, groupBy, sortOn, group)
import Data.Maybe (fromMaybe, mapMaybe, isJust, catMaybes)

type Heightmap = Array (Int, Int) Integer

parser :: Parser Heightmap
parser = liftA2 listArray (\x -> ((0, 0), (length x - 1, length (head x) - 1))) concat <$> manyLines (some digit)

compareNeighbors :: Heightmap -> (Int, Int) -> (Int, Int) -> Ordering
compareNeighbors map x y = (map ! x) `compare` (map ! y)

orderNeighbors :: Heightmap -> (Int, Int) -> [(Int, Int)]
orderNeighbors map (y, x) = sortBy (compareNeighbors map) neighbors
    where
        neighbors = filter valid $ zipWith (\dy dx -> (y + dy, x + dx)) [0, 0, 0, 1, -1] [0, 1, -1, 0, 0]
        valid = inRange (bounds map)

isLowPoint :: Heightmap -> (Int, Int) -> Bool
isLowPoint map c = isAbsoluteLowest && c == fst
    where
         [fst, snd] = take 2 $ orderNeighbors map c
         isAbsoluteLowest = LT == compareNeighbors map fst snd

getLargestBasins :: Heightmap -> [Int]
getLargestBasins heightmap = sortOn negate basinSize
    where
        getBasin c
            | heightmap ! c == 9 = Nothing
            | isLowPoint heightmap c = Just c
            | otherwise = basinTable ! head (orderNeighbors heightmap c)

        basinTable = listArray (bounds heightmap) [getBasin c | c <- range $ bounds heightmap]
        basinSize = map length $ group $ sort $ catMaybes $ elems basinTable

solver :: Heightmap -> IO ()
solver heightmap = do
  putStrLn ">> Part 1"
  print $ sum $ map (\x -> (heightmap ! x) + 1) $ liftA2 filter isLowPoint indices heightmap
  putStrLn ">> Part 2"
  print $ product $ take 3 $ getLargestBasins heightmap

day :: Day
day = dayParse parser solver
