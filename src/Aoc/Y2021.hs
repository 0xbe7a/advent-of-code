module Aoc.Y2021
  ( year
  ) where

import           Aoc.Day
import qualified Aoc.Y2021.D01 as D01
import qualified Aoc.Y2021.D02 as D02
import qualified Aoc.Y2021.D03 as D03
import qualified Aoc.Y2021.D04 as D04
import qualified Aoc.Y2021.D05 as D05
import qualified Aoc.Y2021.D06 as D06
import qualified Aoc.Y2021.D07 as D07
import qualified Aoc.Y2021.D08 as D08
import qualified Aoc.Y2021.D09 as D09
import qualified Aoc.Y2021.D10 as D10
import qualified Aoc.Y2021.D11 as D11


year :: Year
year = Year 2021
  [ ( 1, D01.day),
    ( 2, D02.day),
    ( 3, D03.day),
    ( 4, D04.day),
    ( 5, D05.day),
    ( 6, D06.day),
    ( 7, D07.day),
    ( 8, D08.day),
    ( 9, D09.day),
    ( 10, D10.day),
    ( 11, D11.day)
  ]
