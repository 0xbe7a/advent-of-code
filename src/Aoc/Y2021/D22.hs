{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.D22
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Monad
import Control.Monad.Trans.State
import Data.Maybe (mapMaybe)

data CuboidState = Off | On deriving Eq

data Cuboid = Cuboid AxisRange AxisRange AxisRange

data AxisRange = Range Integer Integer

data Instruction = Instruction CuboidState Cuboid

type CuboidUniverse = ([Cuboid], [Cuboid])

class Intersectable a where
  cardinality :: a -> Integer
  intersection :: a -> a -> Maybe a

instance Intersectable AxisRange where
  cardinality (Range a b) = b - a + 1
  intersection (Range a b) (Range x y)
    | u <= v = Just $ Range u v
    | otherwise  = Nothing
    where
      u = max a x
      v = min b y

instance Intersectable Cuboid where
  cardinality (Cuboid x y z) = product $ map cardinality [x, y, z]
  intersection (Cuboid a b c) (Cuboid x y z) = do
    x' <- intersection a x
    y' <- intersection b y
    z' <- intersection c z
    return $ Cuboid x' y' z'

parser :: Parser [Instruction]
parser = manyLines parseInstructionLine
  where
    parseInstructionLine = Instruction <$> (parseState <* char ' ') <*> parseCuboid
    parseCuboid = Cuboid <$> (string "x=" *> parseRange)
      <*> (char ',' *> string "y=" *> parseRange)
      <*> (char ',' *> string "z=" *> parseRange)

    parseRange = Range <$> parseCoord <* string ".." <*> parseCoord
    parseCoord = signed space decimal
    parseState = (On <$ string "on") <|> (Off <$ string "off")

applyInstructions :: [Instruction] -> State CuboidUniverse Integer
applyInstructions [] = gets $ \(on, off) -> sum (map cardinality on) - sum (map cardinality off)
applyInstructions ((Instruction s r):xs) = do
  turnOn <- gets $ \(_, off) -> mapMaybe (intersection r) off
  turnOff <- gets $ \(on, _) -> mapMaybe (intersection r) on

  if s == On then do
    modify $ \(on, off) -> (r:on ++ turnOn, off ++ turnOff)
  else
    modify $ \(on, off) -> (on ++ turnOn, off ++ turnOff)
  applyInstructions xs

insideBounds :: Cuboid -> Bool
insideBounds (Cuboid x y z) = all insideBounds' [x, y, z]
  where insideBounds' (Range a b) = a >= -50 && b <= 50

solver :: [Instruction] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ evalState (applyInstructions $ filter (\(Instruction _ c) -> insideBounds c) values) ([], [])
  putStrLn ">> Part 2"
  print $ evalState (applyInstructions values) ([], [])

day :: Day
day = dayParse parser solver
