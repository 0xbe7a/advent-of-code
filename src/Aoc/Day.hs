module Aoc.Day
  ( Year(..)
  , Day(..)
  , runDay
  , dayPure
  , dayFile
  , dayString
  , dayText
  , dayParse
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Aoc.Parse

data Year = Year
  { yNum  :: Int
  , yDays :: [(Int, Day)]
  }

data Day
  = DayPure (IO ())
  | DayFile (FilePath -> IO ())

-- | Helper function for trying out days in ghci.
runDay :: Day -> FilePath -> IO ()
runDay (DayPure f) _ = f
runDay (DayFile f) p = f p

dayPure :: IO () -> Day
dayPure = DayPure

dayFile :: (FilePath -> IO ()) -> Day
dayFile = DayFile

dayString :: (FilePath -> String -> IO ()) -> Day
dayString f = dayFile $ \path -> f path =<< readFile path

dayText :: (FilePath -> T.Text -> IO ()) -> Day
dayText f = dayFile $ \path -> f path =<< T.readFile path

dayParse :: Parser a -> (a -> IO ()) -> Day
dayParse p f = dayFile $ \path -> do
  text <- T.readFile path
  parseAndSolve path text p f
