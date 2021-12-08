{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.D08
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Monad
import Data.List (permutations, sort)
import Data.Maybe (isJust, mapMaybe)
import Data.Set (fromAscList, fromList, member)

data Signal = A | B | C | D | E | F | G deriving (Enum, Show, Eq, Ord)

data Line = Line [[Signal]] [[Signal]] deriving (Show, Eq)

parser :: Parser [Line]
parser = manyLines $ Line <$> (signalParser `sepBy` try (char ' ' <* notFollowedBy (char '|'))) <* string " | " <*> (signalParser `sepBy1` char ' ')
  where
    signalParser :: Parser [Signal]
    signalParser =
      some
        ( (A <$ char 'a')
            <|> (B <$ char 'b')
            <|> (C <$ char 'c')
            <|> (D <$ char 'd')
            <|> (E <$ char 'e')
            <|> (F <$ char 'f')
            <|> (G <$ char 'g')
        )

translate :: Enum a => [b] -> [a] -> [b]
translate permutation = map (\x -> permutation !! fromEnum x)

findNumberFromSignals :: [Signal] -> Maybe Int
findNumberFromSignals = matchPattern . sort
  where
    matchPattern [A, B, C, E, F, G] = Just 0
    matchPattern [C, F] = Just 1
    matchPattern [A, C, D, E, G] = Just 2
    matchPattern [A, C, D, F, G] = Just 3
    matchPattern [B, C, D, F] = Just 4
    matchPattern [A, B, D, F, G] = Just 5
    matchPattern [A, B, D, E, F, G] = Just 6
    matchPattern [A, C, F] = Just 7
    matchPattern [A, B, C, D, E, F, G] = Just 8
    matchPattern [A, B, C, D, F, G] = Just 9
    matchPattern _ = Nothing

checkConsistency :: [[Signal]] -> [Signal] -> Bool
checkConsistency signals permutation = all checkSignal signals
  where
    checkSignal :: [Signal] -> Bool
    checkSignal = isJust . findNumberFromSignals . translate permutation

solveLine1 :: Line -> Int
solveLine1 (Line input output) = length $ filter filterDigits $ mapMaybe (findNumberFromSignals . permutate) output
  where
    filterDigits x = (x == 1) || (x == 4) || (x == 7) || (x == 8)
    permutate = translate $ head $ filter (checkConsistency input) (permutations [A .. G])

solveLine2 :: Line -> Int
solveLine2 (Line input output) = snd $ foldl readDecimal (1, 0) $ reverse $ mapMaybe (findNumberFromSignals . permutate) output
  where
    readDecimal (b, acc) d = (b * 10, acc + d * b)
    permutate = translate $ head $ filter (checkConsistency input) (permutations [A .. G])

solver :: [Line] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ sum $ map solveLine1 values
  putStrLn ">> Part 2"
  print $ sum $ map solveLine2 values

day :: Day
day = dayParse parser solver
