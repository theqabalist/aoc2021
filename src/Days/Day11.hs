module Days.Day11 where

import Control.Monad.Writer (execWriter, getSum, runWriter)
import Days.Common.DigitGrid (fromDigitLines)
import Days.Common.Parsing (DigitLines)
import Days.Day11.SquidGame (SquidGame (..), energize, flash, synced)
import Prelude

partOne :: DigitLines -> Integer
partOne = getSum . execWriter . (!! 100) . iterate (>>= flash . energize) . pure . SquidGame . fromDigitLines

partTwo :: DigitLines -> Int
partTwo = length . takeWhile (not . synced . fst . runWriter) . iterate (>>= flash . energize) . pure . SquidGame . fromDigitLines