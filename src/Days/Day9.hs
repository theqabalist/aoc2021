module Days.Day9 where

import Data.List (sort)
import Days.Common.DigitGrid (fromDigitLines)
import Days.Common.Parsing (DigitLines (..))
import Days.Day9.Floor
import Prelude (Int, product, reverse, sum, take, ($), (+), (<$>))

partOne :: DigitLines -> Int
partOne input = sum ((+ 1) <$> lowValues (fromDigitLines input))

partTwo :: DigitLines -> Int
partTwo input =
  let floor = fromDigitLines input
      basins = findBasins floor
   in product $ take 3 $ reverse $ size <$> sort basins