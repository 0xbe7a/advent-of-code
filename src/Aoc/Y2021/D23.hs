{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}


module Aoc.Y2021.D23
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Applicative (Applicative (liftA2))
import Control.Monad
import Data.Ix (Ix (inRange))
import Data.List (delete, foldl')
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.HashPSQ (HashPSQ, empty, fromList, alter, minView)
import Data.Set (Set, fromList, empty, insert, notMember)
import Control.Arrow (first)
import Data.Hashable (Hashable, hash, hashWithSalt)
import GHC.Generics (Generic)

data AmphipodType = A | B | C | D deriving (Enum, Show, Eq, Ord, Generic)

data RoomNames = E | F | G | H deriving (Enum, Eq, Show, Ord, Generic)

data Position = Hallway Integer | Room RoomNames Integer deriving (Show, Eq, Ord, Generic)

data Amphipod = Amphipod {position :: Position, amphipodType :: AmphipodType} deriving (Show, Eq, Ord, Generic)

instance Hashable RoomNames
instance Hashable Position
instance Hashable AmphipodType
instance Hashable Amphipod

energyPerStep :: AmphipodType -> Integer
energyPerStep x = 10 ^ fromEnum x

hallwayCoord :: Position -> Position
hallwayCoord (Room E _) = Hallway 3
hallwayCoord (Room F _) = Hallway 5
hallwayCoord (Room G _) = Hallway 7
hallwayCoord (Room H _) = Hallway 9
hallwayCoord x = x

legalRoom :: AmphipodType -> RoomNames -> Bool
legalRoom A E = True
legalRoom B F = True
legalRoom C G = True
legalRoom D H = True
legalRoom _ _ = False

parser :: Parser [Amphipod]
parser = concat . zipWith (\r p -> map (\(c, t) -> Amphipod (Room r c) t) p) [E .. H] <$> parseRoom `sepEndBy1` newline
  where
    parseRoom = zip [1 ..] <$> some parseAmphipod
    parseAmphipod = A <$ char 'A' <|> B <$ char 'B' <|> C <$ char 'C' <|> D <$ char 'D'

reachable :: [Amphipod] -> Position -> Position -> Maybe Integer
reachable pods s d = case (s, d) of
  (Hallway x, Hallway y) -> if not (any (isBlockingHallway (x, y)) positions) then Just $ abs (x - y) else Nothing
  (Hallway x, Room r c) -> do
    let hallwayEnd = hallwayCoord d
    hallwayDistance <- reachable pods (Hallway x) hallwayEnd
    parkDistance <- if not $ any (isBlockingRoom (Room r c)) positions then Just c else Nothing
    return $ parkDistance + hallwayDistance
  (Room r c, Hallway x) -> reachable pods (Hallway x) (Room r c)
  (Room r c, Room k j) ->
    if r == k
      then Just $ abs j - c
      else do
        let hallwayStart = hallwayCoord s
        unparkDistance <- reachable pods (Room r c) hallwayStart
        remDistance <- reachable pods hallwayStart (Room k j)
        return $ unparkDistance + remDistance
  where
    positions = map position pods
    isBlockingRoom (Room r c) (Room k j) = k == r && j <= c
    isBlockingRoom _ _ = False
    isBlockingHallway (x, y) (Hallway p) = inRange (min x y, max x y) p
    isBlockingHallway _ _ = False

isLegalMove :: [Amphipod] -> Amphipod -> Position -> Bool
isLegalMove pods (Amphipod (Room r c) _) (Hallway x) = x `notElem` [3, 5, 7, 9] && any (foreignPod r) pods
  where foreignPod t (Amphipod (Room k j) o) = k == t && not (legalRoom o k)
        foreignPod _ _ = False
isLegalMove pods (Amphipod (Hallway x) t) (Room r c) = legalRoom t r && not (any (foreignPod t) pods)
  where
    foreignPod t (Amphipod (Room k j) o) = legalRoom t k && not (legalRoom o k)
    foreignPod _ _ = False
isLegalMove pods _ _ = False

isFinished :: [Amphipod] -> Bool
isFinished = all isParkedCorrectly
  where
    isParkedCorrectly (Amphipod (Room r _) g) = legalRoom g r
    isParkedCorrectly _ = False

allPositions :: [Position]
allPositions = [Hallway x | x <- [1 .. 11]] ++ [Room t c | t <- [E .. H], c <- [1 .. 4]]

getAllLegalAndValidMoves :: [Amphipod] -> [(Integer, [Amphipod])]
getAllLegalAndValidMoves state = [(steps * energyPerStep (amphipodType m), newState) |
   m <- state,
   let (lhs, rhs) = break (== m) state,
   let remaining = lhs ++ tail rhs,
   move <- allPositions,
   isLegalMove state m move,
   let newState = lhs ++ Amphipod move (amphipodType m) : tail rhs,
   Just steps <- [reachable remaining (position m) move]]

dijkstra :: [Amphipod] -> ([Amphipod] -> Bool) -> Integer
dijkstra start = dijkstra' frontier explored
  where
    frontier = Data.HashPSQ.fromList [(start, 0, 0)]
    explored = Data.Set.empty

dijkstra' :: HashPSQ [Amphipod] Integer Integer -> Set [Amphipod] -> ([Amphipod] -> Bool) -> Integer
dijkstra' frontier explored end = result
  where
    (curState, curDistance, _, frontier') = fromJust $ minView frontier
    result
      | end curState = curDistance
      | otherwise = dijkstra' frontier'' explored' end

    insertMin q (c, n) = snd $ alter (f c) n q
    f nc m = (0,) $ Just $ first (min nc) $ fromMaybe (nc,0) m

    explored' = insert curState explored
    frontier'' = foldl' insertMin frontier' weightedNeighbors

    weightedNeighbors = map (first (+ curDistance)) $ filter (\(c, s) -> notMember s explored) $ getAllLegalAndValidMoves curState

solver :: [Amphipod] -> IO ()
solver values = do
  putStrLn ">> Part 2"
  print $ dijkstra values isFinished

day :: Day
day = dayParse parser solver