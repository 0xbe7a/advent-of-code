{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.D17 (day) where

import Aoc.Day
import Aoc.Parse
import Control.Applicative (Applicative (liftA2))
import Control.Monad
import Control.Monad.Trans.State (State, evalState, get, gets, put)

import Criterion.Main
import Criterion.Main.Options
import Data.Ix (Ix (inRange))
import Data.List
import Data.Maybe (mapMaybe)

type Bounds = (Integer, Integer)

data TargetArea = Area {x :: Bounds, y :: Bounds} deriving (Show)

data SimState a = State {i :: Integer, p :: Integer, d :: Integer, validSteps :: [Integer], v :: a} deriving (Show)

data SimResult a = Invalid | Valid {steps :: [Integer], finalValue :: a} deriving (Show, Eq)

parser :: Parser TargetArea
parser = Area <$> (string "target area: " *> parseRangeCoord 'x' <* string ", ") <*> parseRangeCoord 'y'
  where
    parseRange = (,) <$> signed space decimal <* string ".." <*> signed space decimal
    parseRangeCoord coord = char coord *> char '=' *> parseRange

simulateAxis :: (SimState a -> Integer) -> (SimState a -> a) -> (Bounds -> SimState a -> Bool) -> Bounds -> State (SimState a) (SimResult a)
simulateAxis velocity value final range = do
  curState <- get
  let i' = i curState + 1
  let p' = p curState + d curState
  let d' = velocity curState
  let v' = value curState
  let isInRange = inRange range p'
  let validSteps' = if isInRange then i' : validSteps curState else validSteps curState
  put $ State i' p' d' validSteps' v'

  finalState <- gets (final range)
  case (finalState, isInRange, null validSteps') of
    (True, True, _) -> return $ Valid (reverse validSteps' ++ [i'..]) v'
    (True, False, True) -> return Invalid
    (True, False, False) -> return $ Valid (reverse validSteps') v'
    _ -> simulateAxis velocity value final range

simulateX :: (Integer, Integer) -> Integer -> SimResult ()
simulateX range x = evalState (simF range) (State 0 0 x [] ())
  where
    simF = simulateAxis (liftA2 (-) d (signum . d)) (const ()) (\_ s -> 0 == d s)

simulateY :: (Integer, Integer) -> Integer -> SimResult Integer
simulateY range y = evalState (simF range) (State 0 0 y [] 0)
  where
    simF = simulateAxis (liftA2 (-) d (const 1)) (liftA2 max p v) (\b s -> d s <= 0 && p s < uncurry min b)

sortedIntersection :: Ord a => [a] -> [a] -> [a]
sortedIntersection [] _ = []
sortedIntersection _ [] = []
sortedIntersection left@(l : ls) right@(r : rs)
  | l == r = l : sortedIntersection ls rs
  | otherwise = sortedIntersection (dropWhile (< l) right) left

combineSimResults :: SimResult a -> SimResult b -> Maybe b
combineSimResults (Valid stepsa _) (Valid stepsb valb)
  | not $ null $ sortedIntersection stepsa stepsb = Just valb
  | otherwise = Nothing
combineSimResults _ _ = Nothing

partialSimulations :: TargetArea -> [(SimResult (), SimResult Integer)]
partialSimulations area = (,) <$> simResults (simulateX (x area)) [0..bound] <*> simResults (simulateY (y area)) [(- bound) .. bound]
  where
    maxAbs a b = abs a `max` abs b
    -- This bound is not correct but holds for actual inputs
    bound = max (uncurry maxAbs (x area)) (uncurry maxAbs (y area))

    simResults simf = filter (Invalid /=) . map simf
    filterY sy = Invalid /= simulateY (y area) sy

solver :: TargetArea -> IO ()
solver area = do
  putStrLn ">> Part 1"
  print $ maximum simResults
  putStrLn ">> Part 2"
  print $ length simResults
  where
    simResults = mapMaybe (uncurry combineSimResults) $ partialSimulations area

benchMain :: TargetArea -> IO ()
benchMain area =
  runMode
    (Run defaultConfig Pattern ["Part 1", "Part 2"])
    [ bench "Part 1" $ whnf (maximum . mapMaybe (uncurry combineSimResults) . partialSimulations) area,
      bench "Part 2" $ whnf (length . mapMaybe (uncurry combineSimResults) . partialSimulations) area
    ]

day :: Day
day = dayParse parser solver