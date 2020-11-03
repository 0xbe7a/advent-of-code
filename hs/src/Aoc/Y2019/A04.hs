module Aoc.Y2019.A04
  ( solve201904
  ) where

type Passwd = (Int, Int, Int, Int, Int, Int)

passwds :: [Passwd]
passwds = do
  a <- [1..9]
  b <- [a..9]
  c <- [b..9]
  d <- [c..9]
  e <- [d..9]
  f <- [e..9]
  pure (a, b, c, d, e, f)

getRange :: Passwd -> Passwd -> [Passwd]
getRange lowerBound upperBound = takeWhile (<=upperBound) $ dropWhile (<lowerBound) passwds

passwdsInRange :: [Passwd]
passwdsInRange = getRange (1,3,6,8,1,8) (6,8,5,9,7,9)

seqDigits :: Passwd -> Bool
seqDigits (a, b, c, d, e, f) = or $ zipWith (==) [a,b,c,d,e] [b,c,d,e,f]

sepSeqDigits :: Passwd -> Bool
sepSeqDigits (a, b, c, d, e, f) =
  let leftEdge  = zipWith (/=) [0,a,b,c,d] [a,b,c,d,e]
      pair      = zipWith (==) [a,b,c,d,e] [b,c,d,e,f]
      rightEdge = zipWith (/=) [b,c,d,e,f] [c,d,e,f,0]
  in  or $ zipWith3 (\x y z -> x && y && z) leftEdge pair rightEdge

solve201904 :: FilePath -> IO ()
solve201904 _ = do
  putStrLn ">> Part 1"
  print $ length $ filter seqDigits passwdsInRange

  putStrLn ">> Part 2"
  print $ length $ filter sepSeqDigits passwdsInRange
