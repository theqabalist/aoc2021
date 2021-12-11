module Days.Day9 where

import Data.Char (digitToInt)
import Data.List (sort)
import Data.Text (Text, lines, unpack)
import Days.Common.DigitGrid (fromDigitLines)
import Days.Common.Parsing (DigitLines (..))
import Days.Day9.Floor
import Lib (Parseable (parse))
import Prelude (Int, fmap, product, reverse, sum, take, ($), (+), (.), (<$>))

partOne :: DigitLines -> Int
partOne input = sum ((+ 1) <$> lowValues (fromDigitLines input))

partTwo :: DigitLines -> Int
partTwo input =
  let floor = fromDigitLines input
      basins = findBasins floor
   in product $ take 3 $ reverse $ size <$> sort basins