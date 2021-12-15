{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Aoc.Y2021.D14
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Applicative (Applicative (liftA2))
import Control.Monad
import Data.List (group, sort)
import Data.Map (Map, assocs, elems, empty, fromList, fromListWith, unionsWith)
import Data.Maybe (fromJust, fromMaybe, isJust, maybeToList)
import GHC.Arr (Ix (range), listArray, (!))

data SubstitutionRule = Rule {left :: Char, right :: Char, ruleProduct :: Char}
type Input = (String, [SubstitutionRule])

parser :: Parser Input
parser = (,) <$> some alphaNumChar <* newline <* newline <*> parseRule `sepBy1` newline
  where
    parseRule = Rule <$> letterChar <*> letterChar <* string " -> " <*> letterChar

allPairs :: [a] -> [(a, a)]
allPairs = liftA2 zip id tail

stringToHist :: String -> Map Char Integer
stringToHist = fromListWith (+) . map (,1)

getProduct :: [SubstitutionRule] -> Char -> Char -> Maybe Char
getProduct rules l r = if not $ null matches then Just $ ruleProduct $ head matches else Nothing
  where
    matches = filter (\p -> left p == l && right p == r) rules

histDyn :: Int -> [SubstitutionRule] -> String -> [(Char, Integer)]
histDyn steps rules = assocs . unionsWith (+) . liftA2 (:) stringToHist (map (uncurry $ stepDyn steps) . allPairs)
  where
    stepDyn :: Int -> Char -> Char -> Map Char Integer
    stepDyn 1 l r = fromList $ map (,1) $ maybeToList $ getProduct rules l r
    stepDyn s l r = unionsWith (+) $ maybe [] (\n -> [dynTable ! (s - 1, l, n), dynTable ! (s - 1, n, r), fromList [(n, 1)]]) $ getProduct rules l r

    dynTable = listArray bounds [stepDyn s l r | (s, l, r) <- range bounds]
    bounds = ((0, 'A', 'A'), (steps, 'Z', 'Z'))

score :: Integral a => [(Char, a)] -> a
score = liftA2 (-) last head . sort . map snd

solver :: Input -> IO ()
solver (template, rules) = do
  putStrLn ">> Part 1"
  print $ score $ histDyn 10 rules template
  putStrLn ">> Part 2"
  print $ score $ histDyn 40 rules template

day :: Day
day = dayParse parser solver