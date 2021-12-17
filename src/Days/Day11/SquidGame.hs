module Days.Day11.SquidGame where

import Control.Monad.Writer (Sum (..), Writer, tell)
import Data.Foldable (foldl')
import Data.Map (insert, toList, update)
import Days.Common.DigitGrid (DigitGrid (..), adjacentLocations')
import Prelude hiding (lookup)

newtype SquidGame = SquidGame DigitGrid
  deriving (Show, Eq)

energize :: SquidGame -> SquidGame
energize (SquidGame grid@DigitGrid {theMap}) = SquidGame $ grid {theMap = fmap (+ 1) theMap}

reset :: (Int, Int) -> SquidGame -> SquidGame
reset loc (SquidGame grid@DigitGrid {theMap}) = SquidGame $ grid {theMap = insert loc 0 theMap}

energizeFromFlash :: (Int, Int) -> SquidGame -> SquidGame
energizeFromFlash loc (SquidGame grid@DigitGrid {theMap}) = SquidGame $ grid {theMap = update (\v -> pure $ if v == 0 || v == 10 then v else v + 1) loc theMap}

flash :: SquidGame -> Writer (Sum Integer) SquidGame
flash game = flash' True game game
  where
    flash' :: Bool -> SquidGame -> SquidGame -> Writer (Sum Integer) SquidGame
    flash' proceed prev curr | not proceed && prev == curr = pure curr
    flash' _ _ curr@(SquidGame grid@DigitGrid {theMap}) = do
      let tens = fst <$> filter ((== 10) . snd) (toList theMap)
      tell (Sum $ fromIntegral $ length tens)
      let resetTens = foldl' (flip reset) curr tens
      let neighbors = tens >>= flip adjacentLocations' grid
      let energized = foldl' (flip energizeFromFlash) resetTens neighbors
      flash' False curr energized

synced :: SquidGame -> Bool
synced (SquidGame DigitGrid {theMap}) = all ((== 0) . snd) (toList theMap)