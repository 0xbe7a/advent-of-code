module Main where

import Options.Applicative

import Aoc.Y2019.A01
import Aoc.Y2019.A02
import Aoc.Y2019.A03
import Aoc.Y2019.A04

data Settings = Settings
  { function :: FilePath -> IO ()
  , filename :: FilePath
  }

solutions :: Parser (FilePath -> IO ())
solutions = subparser $ mconcat $ map (\(name, func) -> command name (info (pure func) mempty))
  [ ("2019_01", solve201901)
  , ("2019_02", solve201902)
  , ("2019_03", solve201903)
  , ("2019_04", solve201904)
  ]

parser :: Parser Settings
parser = Settings
  <$> solutions
  <*> strArgument (metavar "INPUTFILE")

opts :: ParserInfo Settings
opts = info (helper <*> parser) $ fullDesc <> failureCode 1

main :: IO ()
main = do
  settings <- customExecParser (prefs showHelpOnEmpty) opts
  function settings $ filename settings
