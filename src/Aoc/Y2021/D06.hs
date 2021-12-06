module Aoc.Y2021.D06
  ( day
  ) where

import           Control.Monad

import           Aoc.Day
import           Aoc.Parse
import qualified GHC.Arr as Array
import GHC.Arr ((!))

newtype LanternFish = Fish Integer deriving (Show, Eq)

parser :: Parser [LanternFish]
parser = (Fish <$> decimal) `sepBy1` char ','

stepFish :: LanternFish -> [LanternFish]
stepFish (Fish 0) = [Fish 6, Fish 8]
stepFish (Fish timer) = [Fish (timer - 1)]

stepPopulationLength :: Integer -> [LanternFish] -> Integer
stepPopulationLength days = sum . map (stepDyn days)
  where 
    stepDyn 0 _ = 1
    stepDyn d f = sum $ map (\(Fish f) -> dynTable ! (d - 1, f)) $ stepFish f

    dynTable = Array.listArray bounds [stepDyn d (Fish f) | (d, f) <- Array.range bounds]
    bounds = ((0, 0), (days, 8))

solver :: [LanternFish] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ stepPopulationLength 80 values
  putStrLn ">> Part 2"
  print $ stepPopulationLength 256 values

day :: Day
day = dayParse parser solver
