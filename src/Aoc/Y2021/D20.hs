{-# LANGUAGE TupleSections #-}

module Aoc.Y2021.D20
  ( day,
  )
where

import Aoc.Day (Day, dayParse)
import Aoc.Parse (Parser, char, newline, sepEndBy1, some, (<|>))
import Control.Applicative (Applicative (liftA2))
import Data.Bits (Bits (shiftL, (.|.)))
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set ( Set, member, fromAscList, elems, fromDistinctAscList )
import GHC.Arr (Array, Ix (inRange, range), bounds, listArray, (!))

type PatternArray = Array Int Bool

type Image = (Bool, Set (Int, Int))

type Input = (PatternArray, Image)

parser :: Parser Input
parser = (,) <$> (parsePattern <* newline) <*> ((False,) . fromAscList . map fst . filter snd <$> parseImage)
  where
    parsePixel = (False <$ char '.') <|> (True <$ char '#')
    parsePattern = listArray (0, 2 ^ 9 - 1) <$> (some parsePixel <* newline)
    parseImage = liftA2 zip (range . (,) (1, 1) . liftA2 (,) length (length . head)) concat <$> (some parsePixel `sepEndBy1` newline)

dedupOrd :: Eq a => [a] -> [a]
dedupOrd [] = []
dedupOrd (x : xs) = dedupOrd' x xs
  where
    dedupOrd' x [] = [x]
    dedupOrd' x (y : ys) | x == y = dedupOrd' x ys
    dedupOrd' x (y : ys) = x : dedupOrd' y ys

neighbors :: Num a => (a, a) -> [(a, a)]
neighbors (y, x) = zipWith (\dy dx -> (y + dy, x + dx)) [-1, -1, -1, 0, 0, 0, 1, 1, 1] (cycle [-1, 0, 1])

applyPattern :: PatternArray -> Image -> Image
applyPattern pattern (outOfRange, image) = (newOutOfRange, newImage)
  where
    newOutOfRange = if outOfRange then pattern ! (2 ^ 9 - 1) else pattern ! 0
    getNeighborPixels = map (\n -> if member n image then not outOfRange else outOfRange) . neighbors
    getPattern = foldl (\x p -> shiftL x 1 .|. if p then 1 else 0) (0 :: Int) . getNeighborPixels
    kernel c = pattern ! getPattern c

    canidates = dedupOrd . sort . concat $ [k : n | k <- elems image, let n = neighbors k]
    newImage = fromDistinctAscList $ filter (\i -> kernel i /= newOutOfRange) canidates

solver :: Input -> IO ()
solver (pattern, image) = do
  putStrLn ">> Part 1"
  print $ pixelsAfterNstep 2
  putStrLn ">> Part 2"
  print $ pixelsAfterNstep 50
  where
    pixelsAfterNstep 0 = image
    pixelsAfterNstep n = applyPattern pattern $ pixelsAfterNstep (n-1)

day :: Day
day = dayParse parser solver
