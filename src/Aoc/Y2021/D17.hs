{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Y2021.D17 (day) where

import Aoc.Day
import Aoc.Parse
import Control.Applicative (Applicative (liftA2))
import Control.Monad
import Control.Monad.Trans.State (State, evalState, get, gets, put)
import Data.Ix (Ix (inRange))
import Data.List
import Data.Maybe (mapMaybe)

type Bounds = (Integer, Integer)

data TargetArea = Area {x :: Bounds, y :: Bounds} deriving (Show)

data SimState a = State {i :: Integer, p :: Integer, d :: Integer, validSteps :: [Integer], v :: a} deriving (Show)

data SimResult a = Invalid | Valid {infinite :: Bool, steps :: [Integer], finalValue :: a} deriving (Show, Eq)

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
    (True, True, _) -> return $ Valid True (reverse validSteps') v'
    (True, False, True) -> return Invalid
    (True, False, False) -> return $ Valid False (reverse validSteps') v'
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

unionSimResult :: SimResult () -> SimResult Integer -> Maybe Integer
unionSimResult (Valid infa stepsa _) (Valid infb stepsb valb)
  | not $ null $ sortedIntersection stepsa stepsb = Just valb
  | infa && not (null $ sortedIntersection [(head stepsa) ..] stepsb) = Just valb
  | infb && not (null $ sortedIntersection stepsa [(head stepsb) ..]) = Just valb
unionSimResult _ _ = Nothing

simulate :: TargetArea -> Integer -> Integer -> Maybe Integer
simulate area sx sy = unionSimResult simX simY
  where
    simX = simulateX (x area) sx
    simY = simulateY (y area) sy

goodStartConfigs :: TargetArea -> [(Integer, Integer)]
goodStartConfigs area = (,) <$> filter filterX [(- bound) .. bound] <*> filter filterY [(- bound) .. bound]
  where
    maxAbs a b = abs a `max` abs b
    bound = max (uncurry maxAbs (x area)) (uncurry maxAbs (y area))
    filterX sx = Invalid /= simulateX (x area) sx
    filterY sy = Invalid /= simulateY (y area) sy

solver :: TargetArea -> IO ()
solver area = do
  putStrLn ">> Part 1"
  print $ maximum simResults
  putStrLn ">> Part 2"
  print $ length simResults
  where
    simResults = mapMaybe (uncurry (simulate area)) $ goodStartConfigs area

day :: Day
day = dayParse parser solver