module Aoc.Y2021.D12
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Applicative (liftA2)
import Control.Monad

data CaveType = Small | Large deriving (Show, Eq)
data Cave = Cave {cavetype :: CaveType, name :: String} deriving (Show, Eq)
type Edge = (Cave, Cave)

parser :: Parser [Edge]
parser = concat <$> manyLines parseEdge
  where
    flip (a, b) = (b, a)
    parseEdge = liftA2 (:) id ((: []) . flip) <$> ((,) <$> parseCave <* char '-' <*> parseCave)
    parseCave = (Cave Small <$> some lowerChar) <|> (Cave Large <$> some upperChar)

bfs :: Cave -> [Edge] -> Bool -> [Cave] -> Cave -> [[Cave]]
bfs end edges twiceUsed seen pos =
  if pos == end
    then [end : seen] else
    concatMap
    (\x -> bfs end edges (not (withoutTwicePossible seen x) || twiceUsed) (pos : seen) x)
    [b | (a, b) <- edges, a == pos, withoutTwicePossible seen b || (not twiceUsed && (name b /= "start") && (name b /= "end"))]
  where
    withoutTwicePossible seen v = cavetype v == Large || notElem v seen

solver :: [Edge] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ length $ bfs (Cave Small "end") values True [] (Cave Small "start")
  putStrLn ">> Part 2"
  print $ length $ bfs (Cave Small "end") values False [] (Cave Small "start")

day :: Day
day = dayParse parser solver
