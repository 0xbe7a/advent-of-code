module Aoc.Y2019
  ( year
  ) where

import           Aoc.Day
import qualified Aoc.Y2019.D01 as D01
import qualified Aoc.Y2019.D02 as D02
import qualified Aoc.Y2019.D03 as D03
import qualified Aoc.Y2019.D04 as D04
import qualified Aoc.Y2019.D05 as D05
import qualified Aoc.Y2019.D06 as D06

year :: Year
year = Year 2019
  [ ( 1, D01.day)
  , ( 2, D02.day)
  , ( 3, D03.day)
  , ( 4, D04.day)
  , ( 5, D05.day)
  , ( 6, D06.day)
  ]
