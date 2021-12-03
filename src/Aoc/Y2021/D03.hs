module Aoc.Y2021.D03
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Monad
import Debug.Trace
import Data.List (transpose)

data Bit = Zero | One deriving (Enum, Show, Eq)
data Freq = Freq Integer Integer deriving Show

parser :: Parser [[Bit]]
parser = manyLines $ some $ (Zero <$ char '0') <|> (One <$ char '1')

getCommonBits = map (mostFrequentBit . foldl countBit (Freq 0 0)) . transpose
  where
    countBit (Freq a b) Zero  = Freq (a + 1) b
    countBit (Freq a b) One  = Freq a (b + 1)
    mostFrequentBit (Freq a b) = if b >= a then One else Zero

negateBit One = Zero
negateBit Zero = One

getDecimal = getDecimalPower 1 . reverse
  where
    getDecimalPower p [] = 0
    getDecimalPower p [x] = p * fromEnum x
    getDecimalPower p (x : xs) = p * fromEnum x + getDecimalPower (2 * p) xs

filterUntilOne :: [[Bit]] -> (Bit -> Bit) -> [Bit]
filterUntilOne values policy = filterStep (getMask values) values
  where
    getMask = policy . head . getCommonBits
    prune bit values = map tail $ filter ((bit == ) . head) values

    filterStep bit [value] = value
    filterStep bit values =
      let pruned = prune bit values
       in bit : filterStep (getMask pruned) pruned

solver :: [[Bit]] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  let gammaBinary = getCommonBits values
  let epsilonBinary = map negateBit gammaBinary
  let gamma = getDecimal gammaBinary
  let epsilon = getDecimal epsilonBinary

  putStrLn $ show gammaBinary ++ " / " ++ show epsilonBinary
  putStrLn $ show gamma ++ " * " ++ show epsilon ++ " = " ++ show (gamma * epsilon)

  putStrLn ">> Part 2"

  let oxygenBinary = filterUntilOne values id
  let co2Binary = filterUntilOne values negateBit
  let oxygen = getDecimal oxygenBinary
  let co2 = getDecimal co2Binary

  putStrLn $ show oxygenBinary ++ " / " ++ show co2Binary
  putStrLn $ show oxygen ++ " * " ++ show co2 ++ " = " ++ show (oxygen * co2)

day :: Day
day = dayParse parser solver
