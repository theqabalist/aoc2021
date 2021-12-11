module Days.Common.DigitGrid where

import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe)
import Days.Common.Parsing (DigitLines (..))
import Prelude hiding (lookup)

data DigitGrid = DigitGrid
  { width :: Int,
    height :: Int,
    theMap :: Map (Int, Int) Int
  }
  deriving (Show, Eq)

inbounds :: (Int, Int) -> DigitGrid -> Bool
inbounds (x, y) f | x < 0 || y < 0 = False
inbounds (x, y) DigitGrid {width} | x >= width = False
inbounds (x, y) DigitGrid {height} | y >= height = False
inbounds _ _ = True

-- without diagonals
adjacentLocations :: (Int, Int) -> DigitGrid -> [(Int, Int)]
adjacentLocations (x, y) grid = filter (`inbounds` grid) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- with diagonals
adjacentLocations' :: (Int, Int) -> DigitGrid -> [(Int, Int)]
adjacentLocations' t@(x0, y0) grid = filter (t /=) $
  filter (`inbounds` grid) $ do
    x <- [(x0 - 1) .. (x0 + 1)]
    y <- [(y0 - 1) .. (y0 + 1)]
    pure (x, y)

adjacentValues :: (Int, Int) -> DigitGrid -> [Int]
adjacentValues coord grid@DigitGrid {theMap} =
  fromMaybe (error $ "adjacencies to " <> show coord <> " are out of bounds of the map") $
    traverse (`lookup` theMap) $ adjacentLocations coord grid

fromDigitLines :: DigitLines -> DigitGrid
fromDigitLines (DigitLines input) =
  let height = length input
      width = length (head input)
      theMap = fromList $ do
        row <- [0 .. (height - 1)]
        column <- [0 .. (width - 1)]
        pure ((column, row), input !! row !! column)
   in DigitGrid width height theMap
