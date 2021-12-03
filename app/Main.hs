module Main where

import           Control.Monad

import           Options.Applicative

import           Aoc.Day             (Day (..), Year (..))
import qualified Aoc.Y2021           as Y2021

lpad :: a -> Int -> [a] -> [a]
lpad a n as = replicate (n - length as) a ++ as

yearsToParser :: [Year] -> Parser (IO ())
yearsToParser = hsubparser . mconcat . map yearToCommand

yearToCommand :: Year -> Mod CommandFields (IO ())
yearToCommand y = command (show $ yNum y) $ flip info mempty $ daysToParser $ yDays y

daysToParser :: [(Int, Day)] -> Parser (IO ())
daysToParser = hsubparser . mconcat . map (uncurry dayToCommand)

dayToCommand :: Int -> Day -> Mod CommandFields (IO ())
dayToCommand dNum = command (lpad '0' 2 $ show dNum) . flip info mempty . dayToParser

dayToParser :: Day -> Parser (IO ())
dayToParser (DayPure f) = pure f
dayToParser (DayFile f) = f <$> strArgument (metavar "INPUTFILE")

parser :: Parser (IO ())
parser = yearsToParser
  [ Y2021.year
  ]

opts :: ParserInfo (IO ())
opts = info (helper <*> parser) $ fullDesc <> failureCode 1

main :: IO ()
main = join $ customExecParser (prefs showHelpOnEmpty) opts
