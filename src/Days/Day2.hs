module Days.Day2 where

import Data.Either (fromRight)
import Data.Foldable (foldl')
import Days.Day2.Movement
import qualified Days.Day2.Movement.Interp as I
import qualified Days.Day2.Movement.Interp2 as I2
import Days.Day2.Position
import Lib (Parseable (parse))
import Prelude (Int, fmap, ($), (.))

newtype Day2Input = Day2Input [Movement]

instance Parseable Day2Input where
  parse = Day2Input . fromRight [Stay] . parseMovements

partOne :: Day2Input -> Int
partOne (Day2Input input) = finalize $ foldl' (\p next -> next p) (Position 0 0 0) (fmap I.compile input)

partTwo :: Day2Input -> Int
partTwo (Day2Input input) = finalize $ foldl' (\p next -> next p) (Position 0 0 0) (fmap I2.compile input)