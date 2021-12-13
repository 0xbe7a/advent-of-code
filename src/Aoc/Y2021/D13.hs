{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.D13
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Monad
import Data.Function (on)
import Data.List (groupBy, nub, sort)
import GHC.Arr (accumArray, assocs)

data Point = Point {x :: Integer, y :: Integer} deriving (Show, Eq, Ord)

data FoldDirection = X | Y deriving (Show, Eq)

data FoldInstruction = FoldInstruction {direction :: FoldDirection, coordinate :: Integer} deriving (Show)

type FoldBundle = ([Point], [FoldInstruction])

parser :: Parser FoldBundle
parser = (,) <$> parsePoint `endBy1` newline <* newline <*> parseDirection `sepEndBy1` newline
  where
    parsePoint = Point <$> decimal <* char ',' <*> decimal
    parseDirection = FoldInstruction <$> (string "fold along " *> ((X <$ char 'x') <|> (Y <$ char 'y'))) <* char '=' <*> decimal

foldPoint :: FoldInstruction -> Point -> Point
foldPoint (FoldInstruction X c) p = Point (c - abs (c - x p)) (y p)
foldPoint (FoldInstruction Y c) p = Point (x p) (c - abs (c - y p))

foldPoints :: [Point] -> FoldInstruction -> [Point]
foldPoints points instr = nub $ sort $ map (foldPoint instr) points

generateGrid :: [Point] -> String
generateGrid points =
  unlines $
    map (map snd) $
      groupBy ((==) `on` fst . fst) $
        assocs $
          accumArray
            (\_ a -> a)
            ' '
            ((0, 0), bounds)
            $ map (\p -> ((y p, x p), '#')) points
  where
    bounds = (maximum $ map y points, maximum $ map x points)

solver :: FoldBundle -> IO ()
solver (points, instructions) = do
  putStrLn ">> Part 1"
  print $ length $ foldPoints points (head instructions)
  putStrLn ">> Part 2"
  putStrLn $ generateGrid $ foldl foldPoints points instructions

day :: Day
day = dayParse parser solver
