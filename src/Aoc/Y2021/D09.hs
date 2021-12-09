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
import Data.Either (isLeft)

type Heightmap = Array (Int, Int) Integer

parser :: Parser Heightmap
parser = liftA2 listArray ((,) (1, 1) . liftA2 (,) length (length . head)) concat <$> manyLines (some digit)

smallestNeighbors :: Heightmap -> (Int, Int) -> [(Int, Int)]
smallestNeighbors heightmap (y, x) = head $ groupBy (\x y -> EQ == compareNeighbors x y) $ sortBy compareNeighbors neighbors
    where
        compareNeighbors x y = (heightmap ! x) `compare` (heightmap ! y)
        neighbors = filter valid $ zipWith (\dy dx -> (y + dy, x + dx)) [0, 0, 0, 1, -1] [0, 1, -1, 0, 0]
        valid = inRange (bounds heightmap)

isAbsoluteLowest :: (Int, Int) -> [(Int, Int)] -> Bool
isAbsoluteLowest c = liftA2 (&&) ((==) c . head) ((==) 1 . length)

getLargestBasins :: Heightmap -> [Int]
getLargestBasins heightmap = sortOn negate basinSize
    where
        getBasin c
            | heightmap ! c == 9 = Nothing
            | isAbsoluteLowest c lpoints = Just smallestNeighbor
            | otherwise = basinTable ! smallestNeighbor
            where
              smallestNeighbor = head lpoints
              lpoints = smallestNeighbors heightmap c

        basinTable = listArray (bounds heightmap) [getBasin c | c <- range $ bounds heightmap]
        basinSize = map length $ group $ sort $ catMaybes $ elems basinTable

solver :: Heightmap -> IO ()
solver heightmap = do
  putStrLn ">> Part 1"
  print $ sum $ map (\x -> (heightmap ! x) + 1) $ filter (liftA2 ($) isAbsoluteLowest (smallestNeighbors heightmap)) $ indices heightmap
  putStrLn ">> Part 2"
  print $ product $ take 3 $ getLargestBasins heightmap

day :: Day
day = dayParse parser solver