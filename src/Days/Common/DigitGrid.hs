module Days.Common.DigitGrid where

import Data.List (sort)
import Data.Map (Map, empty, fromList, lookup, toList, union)
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
inbounds (x, y) _ | x < 0 || y < 0 = False
inbounds (x, _) DigitGrid {width} | x >= width = False
inbounds (_, y) DigitGrid {height} | y >= height = False
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

valueAt :: (Int, Int) -> DigitGrid -> Int
valueAt p DigitGrid {theMap} = fromMaybe (error (show p <> " is not in the grid")) $ lookup p theMap

join :: DigitGrid -> DigitGrid -> DigitGrid
join (DigitGrid w1 h1 map1) (DigitGrid w2 h2 map2) = DigitGrid (max w1 w2) (max h1 h2) (map1 `union` map2)

tileWith :: Int -> Int -> ((Int, Int, Int, Int) -> [((Int, Int), Int)] -> [((Int, Int), Int)]) -> DigitGrid -> DigitGrid
tileWith x y modifyGrid DigitGrid {width, height, theMap} =
  let grids = do
        i <- [0 .. (x - 1)]
        j <- [0 .. (y - 1)]
        pure $ modifyGrid (i * width, j * height, i, j) (sort $ toList theMap)
      bigGrid = foldr union empty (fromList <$> grids)
      maxX = width * x
      maxY = height * y
   in DigitGrid maxX maxY bigGrid

fromDigitLines :: DigitLines -> DigitGrid
fromDigitLines (DigitLines input) =
  let height = length input
      width = length (head input)
      theMap = fromList $ do
        row <- [0 .. (height - 1)]
        column <- [0 .. (width - 1)]
        pure ((column, row), input !! row !! column)
   in DigitGrid width height theMap
