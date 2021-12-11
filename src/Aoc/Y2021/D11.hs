{-# LANGUAGE TupleSections #-}
module Aoc.Y2021.D11
  ( day
  ) where

import           Control.Monad

import           Aoc.Day
import           Aoc.Parse
import GHC.Arr (Array, listArray, bounds, elems, assocs, inRange, (!), (//), accumArray, accum, STArray)
import Control.Applicative (liftA2)
import Control.Monad.Trans.State (State, modify, get, gets, evalState, execState, runState)

data Octopus = Depleted | Active Int deriving (Show)
type Point = (Int, Int)
type Gamestate = Array Point Octopus

parser :: Parser Gamestate
parser = liftA2 listArray ((,) (1, 1) . liftA2 (,) length (length . head)) concat <$> manyLines (some (Active <$> digit))

stepOctopus :: Octopus -> Octopus
stepOctopus (Active x) = Active (x + 1)
stepOctopus Depleted = Depleted

rechargeDepleted :: Octopus -> Octopus
rechargeDepleted Depleted = Active 0
rechargeDepleted x = x

updatePoints :: (Octopus -> Octopus) -> [Point] -> Gamestate -> Gamestate
updatePoints f points state = accum (\s _ -> f s) state (map (,1) points)

updateAll :: (Octopus -> Octopus) -> Gamestate -> Gamestate
updateAll f = liftA2 listArray bounds (map f . elems)

step :: State Gamestate Int
step = do
    modify $ updateAll stepOctopus
    flashesCount <- flash
    modify $ updateAll rechargeDepleted
    return flashesCount

isExcited :: Octopus -> Bool
isExcited (Active x) = x >= 10
isExcited Depleted = False

neighbors :: (Point, Point) -> Point -> [Point]
neighbors bounds (y, x) = filter (inRange bounds) [(y+dy, x+dx) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

flash :: State Gamestate Int
flash = do
    excitedOctopi <- gets $ map fst . filter (isExcited . snd) . assocs
    fieldBounds <- gets bounds

    let nDirectFlashes = length excitedOctopi
    if nDirectFlashes == 0 then return 0 else do
        modify $ updatePoints (const Depleted) excitedOctopi

        let flashNeighbors = concatMap (neighbors fieldBounds) excitedOctopi
        modify $ updatePoints stepOctopus flashNeighbors

        indirectFlashes <- flash
        return $ nDirectFlashes + indirectFlashes

iterateUntilM :: State Gamestate Int -> (Int -> Bool) -> Gamestate -> [Int]
iterateUntilM iter p s = if p v then [v] else v : iterateUntilM iter p s'
    where (v, s') = runState iter s

solver :: Gamestate -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ sum $ take 100 $ iterateUntilM step (const False) values
  putStrLn ">> Part 2"
  print $ length $ iterateUntilM step (== length (elems values)) values

day :: Day
day = dayParse parser solver
