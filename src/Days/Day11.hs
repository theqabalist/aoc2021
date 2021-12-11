module Days.Day11 where

import Control.Monad (join)
import Control.Monad.Writer (execWriter, getSum, runWriter)
import Data.Text (Text)
import Days.Common.DigitGrid (DigitGrid (..), fromDigitLines)
import Days.Common.Parsing (DigitLines)
import Days.Day11.SquidGame (SquidGame (..), energize, flash, synced)
import Prelude

partOne :: DigitLines -> Integer
partOne = getSum . execWriter . (!! 100) . iterate (>>= flash . energize) . pure . SquidGame . fromDigitLines

partTwo :: DigitLines -> Int
partTwo = length . takeWhile (not . synced . fst . runWriter) . iterate (>>= flash . energize) . pure . SquidGame . fromDigitLines