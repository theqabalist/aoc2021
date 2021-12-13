module Days.Day13 where

import Data.Foldable (foldl')
import Data.Text (Text, splitOn)
import Days.Day13.CoordBag
import Days.Day13.Day13Input (Day13Input (..))
import Debug.Trace (traceShowId)
import Lib (Parseable (parse))
import Prelude

partOne :: Day13Input -> Int
partOne (Day13Input (bag, folds)) = visibleDots $ foldl' (flip fold) bag (take 1 folds)

partTwo :: Day13Input -> String
partTwo (Day13Input (bag, folds)) = decodeBag $ foldl' (flip fold) bag folds