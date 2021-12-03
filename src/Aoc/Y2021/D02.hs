{-# LANGUAGE OverloadedStrings #-}
module Aoc.Y2021.D02
  ( day
  ) where

import           Control.Monad
import           Control.Applicative
import           Data.Functor

import           Aoc.Day
import Aoc.Parse
    ( decimal, manyLines, Parser, char, string, signed )

data Direction = Forward | Down | Up
  deriving (Show, Eq, Ord)

data Movement = Movement Direction Integer
  deriving (Show, Eq, Ord)

parser :: Parser [Movement]
parser = manyLines $ Movement <$> (pDirection <* char ' ') <*> signed (pure ()) decimal
  where
    pDirection = (Forward <$ string "forward") <|> (Down <$ string "down") <|> (Up <$ string "up")

applyMovementPart1 (x, y) (Movement direction steps) = case direction of
  Forward -> (x + steps, y)
  Down -> (x, y + steps)
  Up -> (x, y - steps)

applyMovementPart2 (aim, x, y) (Movement direction steps) = case direction of
  Forward -> (aim, x + steps, y + steps * aim)
  Down -> (aim + steps, x, y)
  Up -> (aim - steps, x, y)

solver :: [Movement] -> IO ()
solver directions = do
  putStrLn ">> Part 1"
  let (x1, y1) = foldl applyMovementPart1 (0, 0) directions
  putStrLn $ show x1 ++ " * " ++ show y1 ++ " = " ++ show (x1 * y1)
  putStrLn ">> Part 2"
  let (_, x2, y2) = foldl applyMovementPart2 (0, 0, 0) directions
  putStrLn $ show x2 ++ " * " ++ show y2 ++ " = " ++ show (x2 * y2)

day :: Day
day = dayParse parser solver
