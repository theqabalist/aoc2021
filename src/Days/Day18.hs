module Days.Day18 where

import Data.Foldable (foldl')
import Data.Text (lines)
import Days.Day18.SFPair
import Lib (Parseable (parse))
import Prelude hiding (lines)

newtype Day18Input = Day18Input [SFPair]
  deriving (Show)

instance Parseable Day18Input where
  parse = Day18Input . fmap parse . lines

partOne :: Day18Input -> Int
partOne (Day18Input []) = error "not enough input"
partOne (Day18Input [input]) = magnitude $ reduce input
partOne (Day18Input (p : ps)) = magnitude $ foldl' (.+.) p ps

partTwo :: Day18Input -> Int
partTwo (Day18Input []) = error "not enough input"
partTwo (Day18Input [input]) = magnitude $ reduce input
partTwo (Day18Input ps) = foldr max 0 $ do
  x <- ps
  y <- ps
  if x == y then [] else pure $ magnitude (x .+. y)