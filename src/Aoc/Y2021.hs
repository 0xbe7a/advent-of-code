module Aoc.Y2021
  ( year
  ) where

import           Aoc.Day
import qualified Aoc.Y2021.D01 as D01
import qualified Aoc.Y2021.D02 as D02
import qualified Aoc.Y2021.D03 as D03
import qualified Aoc.Y2021.D04 as D04

year :: Year
year = Year 2021
  [ ( 1, D01.day),
    ( 2, D02.day),
    ( 3, D03.day),
    ( 4, D04.day)
  ]
