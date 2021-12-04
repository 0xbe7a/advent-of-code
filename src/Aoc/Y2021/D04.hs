module Aoc.Y2021.D04
  ( day
  ) where

import           Control.Monad
import           Control.Applicative
import           Aoc.Day
import           Aoc.Parse
import Data.List (transpose, partition)
import Data.Either (fromLeft, isRight)

data BingoGame = Game [Integer] [BingoTable] deriving Show

type BingoTable = [[BingoCell]]
type BingoCell = Either Integer Integer

parser :: Parser BingoGame
parser = Game <$> (draws <* newline <* newline) <*> (bingoTable `sepEndBy1` newline)
  where
    number = optional (char ' ') *> (Left <$> decimal)
    draws = decimal `sepBy1` char ','
    bingoLine = number `sepBy1` char ' '
    bingoTable = bingoLine `sepEndBy1` newline

markNumberOnBoard number = map $ map markNumber
  where
    markNumber n = if n == Left number then Right number else n

checkWinning :: BingoTable -> Bool
checkWinning = liftA2 (||) checkBoardRows (checkBoardRows . transpose)
  where
    checkBoardRows =  any $ all isRight

getTotalUnmarkedScore :: BingoTable -> Integer
getTotalUnmarkedScore = sum . map (sum . map (fromLeft 0))

getWinningScore :: ([BingoTable] -> [BingoTable] -> Bool) -> [Integer] -> [BingoTable] -> Integer
getWinningScore _ [] _ = 0
getWinningScore winPolicy (d:ds) boards
  | winPolicy winningBoards playingBoards = d * getTotalUnmarkedScore (head winningBoards)
  | otherwise = getWinningScore winPolicy ds playingBoards
    where 
          nextBoards = map (markNumberOnBoard d) boards
          (winningBoards, playingBoards) = partition checkWinning nextBoards

solver :: BingoGame -> IO ()
solver (Game draws boards) = do
  putStrLn ">> Part 1"
  print $ getWinningScore (\w l -> not $ null w) draws boards

  putStrLn ">> Part 2"
  print $ getWinningScore (\w l -> null l) draws boards

day :: Day
day = dayParse parser solver
