{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.D19
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
    ( char,
      newline,
      space,
      decimal,
      signed,
      string,
      between,
      sepBy1,
      sepEndBy1,
      Parser )
import Control.Monad
import Data.Foldable (foldl', maximumBy)
import Data.Set (Set, fromList, intersection, map, size, toList, union)

data Point = Point {x :: Integer, y :: Integer, z :: Integer} deriving (Show, Eq, Ord)

data Scanner = Scanner {index :: Integer, beacons :: Set Point} deriving (Show)

parser :: Parser [Scanner]
parser = parseScanner `sepBy1` newline
  where
    parseScanner = Scanner <$> (parseScannerHeader <* newline) <*> parsePoints
    parseCoord = signed space decimal
    parsePoints = fromList <$> parsePoint `sepEndBy1` newline
    parsePoint = Point <$> (parseCoord <* char ',') <*> (parseCoord <* char ',') <*> parseCoord
    parseScannerHeader = between (string "--- ") (string " ---") (string "scanner " *> decimal)

-- https://www.euclideanspace.com/maths/discrete/groups/categorise/finite/cube/index.htm
rotations :: [Point -> Point]
rotations =
  [ id,
    x,
    y,
    x . x,
    x . y,
    y . x,
    y . y,
    x . x . x,
    x . x . y,
    x . y . x,
    x . y . y,
    y . x . x,
    y . y . x,
    y . y . x,
    y . y . x,
    y . y . y,
    x . x . x . y,
    x . x . y . x,
    x . x . y . y,
    x . y . x . x,
    x . y . y . y,
    y . x . x . x,
    y . y . y . x,
    x . x . x . y . x,
    x . y . x . x . x,
    x . y . y . y . x
  ]
  where
    x (Point xp yp zp) = Point xp (- zp) yp
    y (Point xp yp zp) = Point zp yp (- xp)

overlap :: Scanner -> Scanner -> Maybe (Point, Scanner)
overlap (Scanner i referencePoints) (Scanner _ otherPoints) = case remappedPoints of
  (remappedPoints, offset) : _ -> Just (offset, Scanner i $ union referencePoints remappedPoints)
  _ -> Nothing
  where
    overlapSize = 12
    add a b = Point (x a + x b) (y a + y b) (z a + z b)
    sub a b = Point (x a - x b) (y a - y b) (z a - z b)
    remappedPoints =
      [ (remappedPoints, offset)
        | alignmentPoint <- take (size referencePoints - (overlapSize - 1)) $ toList referencePoints,
          rot <- rotations,
          alignmentCanidate <- toList otherPoints,
          let offset = sub alignmentPoint (rot alignmentCanidate),
          let remappedPoints = Data.Set.map (add offset . rot) otherPoints,
          size (intersection referencePoints remappedPoints) >= overlapSize
      ]

combine :: [Scanner] -> ([Point], Scanner)
combine scanners = fst $ combine' (([], head scanners), tail scanners)
  where
    combine' (ref, []) = (ref, [])
    combine' (ref, others) = combine' $ foldl' combineFn (ref, []) others
    combineFn (ref, misfits) other = maybe (ref, other : misfits) (\(offset, nref) -> ((offset : fst ref, nref), misfits)) (overlap (snd ref) other)

maxDistance :: [Point] -> Integer
maxDistance points = maximum $ Prelude.map (\x -> maximum $ Prelude.map (manhatten x) points) points
  where
    manhatten a b = abs (x a - x b) + abs (y a - y b) + abs (z a - z b)

solver :: [Scanner] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  let aligned = combine values
  print $ length $ toList $ beacons $ snd aligned
  putStrLn ">> Part 2"
  print $ maxDistance $ fst aligned

day :: Day
day = dayParse parser solver
