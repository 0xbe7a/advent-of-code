{-# LANGUAGE TupleSections #-}

module Aoc.Y2021.D20
  ( day,
  )
where

import Aoc.Day ( dayParse, Day )
import Aoc.Parse ( char, newline, sepEndBy1, some, (<|>), Parser )
import Control.Applicative (Applicative (liftA2))
import Data.Bits ( Bits((.|.), shiftL) )
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Arr (Array, Ix (inRange, range), bounds, listArray, (!))

type PatternArray = Array Int Bool

type Image = (Bool, Map.Map (Int, Int) Bool)

type Input = (PatternArray, Image)

parser :: Parser Input
parser = (,) <$> (parsePattern <* newline) <*> parseImage
  where
    parsePixel = (False <$ char '.') <|> (True <$ char '#')
    parsePattern = listArray (0, 2 ^ 9 - 1) <$> (some parsePixel <* newline)
    parseImage = (False,) . Map.fromAscList . liftA2 zip (range . (,) (1, 1) . liftA2 (,) length (length . head)) concat <$> (some parsePixel `sepEndBy1` newline)

dedupOrd :: Eq a => [a] -> [a]
dedupOrd [] = []
dedupOrd (x : xs) = dedupOrd' x xs
  where
    dedupOrd' x [] = [x]
    dedupOrd' x (y : ys) | x == y = dedupOrd' x ys
    dedupOrd' x (y : ys) = x : dedupOrd' y ys

applyPattern :: PatternArray -> Image -> Image
applyPattern pattern (outOfRange, image) = (newOutOfRange, newImage)
  where
    newOutOfRange = if outOfRange then pattern ! (2 ^ 9 - 1) else pattern ! 0
    neighbors (y, x) = zipWith (\dy dx -> (y + dy, x + dx)) [-1, -1, -1, 0, 0, 0, 1, 1, 1] (cycle [-1, 0, 1])
    getNeighborPixels = map (\n -> Map.findWithDefault outOfRange n image) . neighbors
    getPattern = foldl (\x p -> shiftL x 1 .|. if p then 1 else 0) (0 :: Int) . getNeighborPixels
    kernel c = pattern ! getPattern c

    canidates = dedupOrd . sort . concat $ [k : n | k <- Map.keys image, let n = neighbors k]
    newImage = Map.fromAscList $ filter (\x -> snd x /= newOutOfRange) $ map (\i -> (i, kernel i)) canidates

solver :: Input -> IO ()
solver (pattern, image) = do
  putStrLn ">> Part 1"
  print $ pixelsAfterNstep 2
  putStrLn ">> Part 2"
  print $ pixelsAfterNstep 50
  where
    pixelsAfterNstep n = length $ snd $ iterate (applyPattern pattern) image !! n

day :: Day
day = dayParse parser solver
