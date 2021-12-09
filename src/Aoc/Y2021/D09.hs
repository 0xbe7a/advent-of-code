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
smallestNeighbors heightmap (y, x) = head $ groupBy (\x y -> heightmap ! x == heightmap ! y) $ sortBy compareNeighbors neighbors
    where
        compareNeighbors x y = (heightmap ! x) `compare` (heightmap ! y)
        neighbors = filter valid $ zipWith (\dy dx -> (y + dy, x + dx)) [0, 0, 0, 1, -1] [0, 1, -1, 0, 0]
        valid = inRange (bounds heightmap)

-- Left if c is a lowest point, Right if there is a lower point nearby
getLowestPoint :: Heightmap -> (Int, Int) -> Either (Int, Int) (Int, Int)
getLowestPoint heightmap c = if isAbsoluteLowest c lowestNeighbor then Left c else Right (head lowestNeighbor)
  where
    lowestNeighbor = smallestNeighbors heightmap c
    isAbsoluteLowest c = liftA2 (&&) ((==) c . head) ((==) 1 . length)

getLargestBasins :: Heightmap -> [Int]
getLargestBasins heightmap = sortOn negate basinSize
    where
        getBasin c
            | heightmap ! c == 9 = Nothing
            | Left d <- lp = Just d
            | Right d <- lp = basinTable ! d
            where lp = getLowestPoint heightmap c

        basinTable = listArray (bounds heightmap) [getBasin c | c <- range $ bounds heightmap]
        basinSize = map length $ group $ sort $ catMaybes $ elems basinTable

solver :: Heightmap -> IO ()
solver heightmap = do
  putStrLn ">> Part 1"
  print $ sum $ map (\x -> (heightmap ! x) + 1) $ filter (isLeft . getLowestPoint heightmap) $ indices heightmap
  putStrLn ">> Part 2"
  print $ product $ take 3 $ getLargestBasins heightmap

day :: Day
day = dayParse parser solver
