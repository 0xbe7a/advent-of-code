module Aoc.Y2021.D18
  ( day,
  )
where

import Aoc.Day
import Aoc.Parse
import Control.Monad
import Data.Char (digitToInt)
import Data.Complex (magnitude)
import Data.Either (isLeft)

data Expr a = Literal a | Pair (Expr a) (Expr a) deriving (Show, Eq)

parser :: Parser [Expr Int]
parser = parseExpr `sepEndBy1` newline
  where
    parseExpr = parsePair <|> parseLit
    parsePair = Pair <$> (char '[' *> parseExpr <* char ',') <*> (parseExpr <* char ']')
    parseLit = Literal <$> decimal

explode :: Num a => Expr a -> Either (Expr a) (Expr a)
explode e = either Left (\(_, x', _) -> Right x') (explode' e 0)
  where
    addLeft (Literal x) n = Literal (x + n)
    addLeft (Pair x y) n = Pair (addLeft x n) y
    addRight (Literal x) n = Literal (x + n)
    addRight (Pair x y) n = Pair x (addRight y n)

    explode' (Literal x) _ = Left (Literal x)
    explode' (Pair (Literal l) (Literal r)) d | d >= 4 = Right (l, Literal 0, r)
    explode' (Pair x y) d = case explode' x (d + 1) of
      Right (a, n, b) -> Right (a, Pair n (addLeft y b), 0)
      _ -> case explode' y (d + 1) of
        Right (a, n, b) -> Right (0, Pair (addRight x a) n, b)
        _ -> Left (Pair x y)

divRoundUp :: Integral p => p -> p -> p
divRoundUp a b = if rem /= 0 then d + 1 else d where (d, rem) = quotRem a b

split :: (Integral a, Ord a) => Expr a -> Either (Expr a) (Expr a)
split (Literal x) | x >= 10 = Right (Pair (Literal (x `div` 2)) (Literal (x `divRoundUp` 2)))
split (Pair x y) = case split x of
  Right n -> Right (Pair n y)
  _ -> case split y of
    Right n -> Right (Pair x n)
    _ -> Left (Pair x y)
split x = Left x

reduce :: (Integral a, Ord a) => Expr a -> Expr a
reduce = either id reduce . either split Right . explode

snailAdd :: Integral a => Expr a -> Expr a -> Expr a
snailAdd x y = reduce $ Pair x y

snailMagnitude :: Num p => Expr p -> p
snailMagnitude (Literal a) = a
snailMagnitude (Pair x y) = 3 * snailMagnitude x + 2 * snailMagnitude y

solver :: [Expr Int] -> IO ()
solver values = do
  putStrLn ">> Part 1"
  print $ snailMagnitude $ foldl1 snailAdd values
  putStrLn ">> Part 2"
  print $ maximum $ map (snailMagnitude . uncurry snailAdd) $ filter (uncurry (/=)) $ (,) <$> values <*> values

day :: Day
day = dayParse parser solver
