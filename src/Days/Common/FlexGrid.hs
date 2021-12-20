module Days.Common.FlexGrid where

import Data.Map (Map, delete, fromList, lookup, toList, union)
import Data.Maybe (fromMaybe)
import Days.Common.Parsing (FlexLines (..))
import Debug.Trace
import Prelude hiding (lookup)

data FlexGrid a = FlexGrid
  { minX :: Int,
    minY :: Int,
    width :: Int,
    height :: Int,
    theMap :: Map (Int, Int) a
  }
  deriving (Show, Eq)

inbounds :: forall a. (Int, Int) -> FlexGrid a -> Bool
inbounds (x, y) FlexGrid {minX, minY} | x < minX || y < minY = False
inbounds (x, _) FlexGrid {minX, width} | x >= minX + width = False
inbounds (_, y) FlexGrid {minY, height} | y >= minY + height = False
inbounds _ _ = True

-- without diagonals
adjacentLocations :: forall a. (Int, Int) -> FlexGrid a -> [(Int, Int)]
adjacentLocations (x, y) grid = filter (`inbounds` grid) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- with diagonals
adjacentLocations' :: forall a. (Int, Int) -> FlexGrid a -> [(Int, Int)]
adjacentLocations' t@(x0, y0) grid = filter (t /=) $
  filter (`inbounds` grid) $ do
    x <- [(x0 - 1) .. (x0 + 1)]
    y <- [(y0 - 1) .. (y0 + 1)]
    pure (x, y)

adjacentValues :: forall a. (Int, Int) -> FlexGrid a -> [a]
adjacentValues coord grid@FlexGrid {theMap} =
  fromMaybe (error $ "adjacencies to " <> show coord <> " are out of bounds of the map") $
    traverse (`lookup` theMap) $ adjacentLocations coord grid

valueAt :: forall a. ((Int, Int) -> a) -> (Int, Int) -> FlexGrid a -> a
valueAt produce p g@FlexGrid {theMap} = if not (inbounds p g) then error (show p <> " is out of bounds") else fromMaybe (produce p) $ lookup p theMap

valueAt' :: forall a. (Monoid a) => (Int, Int) -> FlexGrid a -> a
valueAt' p FlexGrid {theMap} = fromMaybe mempty $ lookup p theMap

convolute' :: forall a. (Show a, Monoid a) => (Int, Int) -> ([a] -> a) -> FlexGrid a -> FlexGrid a
convolute' (kernelX, kernelY) _ _ | even kernelX || even kernelY = error "incorrect kernel specified, must be odd dimensions"
convolute' (kernelX, kernelY) tx g@(FlexGrid {theMap}) =
  let xrange x =
        let split = floor $ ((fromIntegral kernelX :: Double) - 1) / 2
         in [x - split .. x + split]
      yrange y =
        let split = floor $ ((fromIntegral kernelY :: Double) - 1) / 2
         in [y - split .. y + split]
      neighborhood (x, y) = do
        y0 <- yrange y
        x0 <- xrange x
        pure $ valueAt' ((x0, y0)) g
      convoluted = fromList $ (\(k, _) -> (k, tx (neighborhood k))) <$> toList theMap
   in g {theMap = convoluted}

convolute1 :: forall a. (Monoid a) => Int -> (Int, Int) -> ([a] -> a) -> (Int, Int) -> FlexGrid a -> a
convolute1 _ (kernelX, kernelY) _ _ _ | even kernelX || even kernelY = error "incorrect kernel specified, must be odd dimensions"
convolute1 depth k@(kernelX, kernelY) tx coord g@(FlexGrid {theMap}) =
  let xrange x =
        let split = floor $ ((fromIntegral kernelX :: Double) - 1) / 2
         in [x - split .. x + split]
      yrange y =
        let split = floor $ ((fromIntegral kernelY :: Double) - 1) / 2
         in [y - split .. y + split]
      neighborhood (x, y) = do
        y0 <- yrange y
        x0 <- xrange x
        pure $
          if depth == 0
            then valueAt' (x0, y0) g
            else convolute1 (depth - 1) k tx (x0, y0) g
   in tx $ neighborhood coord

join :: forall a. FlexGrid a -> FlexGrid a -> FlexGrid a
join (FlexGrid mx1 my1 w1 h1 map1) (FlexGrid mx2 my2 w2 h2 map2) = FlexGrid (min mx1 mx2) (min my1 my2) (max w1 w2) (max h1 h2) (map1 `union` map2)

padRing :: forall a. Show a => ((Int, Int) -> a) -> FlexGrid a -> FlexGrid a
padRing padding (FlexGrid minX minY w h theMap) =
  let newMinX = minX - 1
      newMaxX = minX + w
      newMinY = minY - 1
      newMaxY = minY + h
      fill = (\coord -> (coord, padding coord))
      top = fill . (,newMinY) <$> [newMinX .. newMaxX - 1]
      right = fill . (newMaxX,) <$> [newMinY .. newMaxY - 1]
      bottom = fill . (,newMaxY) <$> [newMinX + 1 .. newMaxX]
      left = fill . (newMinX,) <$> [newMinY + 1 .. newMaxY]
   in FlexGrid newMinX newMinY (w + 2) (h + 2) (theMap `union` fromList (top ++ right ++ bottom ++ left))

depad :: forall a. FlexGrid a -> FlexGrid a
depad (FlexGrid minX minY w h theMap) =
  let newMinX = minX + 1
      newMinY = minY + 1
      newWidth = w - 2
      newHeight = h - 2
      top = (,minY) <$> [minX .. minX + (w - 1)]
      right = (minX + (w - 1),) <$> [minY .. minY + (h - 1)]
      bottom = (,minY + (h - 1)) <$> [minX .. minX + (w - 1)]
      left = (minX,) <$> [minY .. minY + (h - 1)]
      removed = foldr delete theMap (top ++ right ++ bottom ++ left)
   in (FlexGrid newMinX newMinY newWidth newHeight removed)

padRing' :: forall a. (Monoid a, Show a) => FlexGrid a -> FlexGrid a
padRing' = padRing (const mempty)

fromLines :: forall a. FlexLines a -> FlexGrid a
fromLines (FlexLines input) =
  let height = length input
      width = length (head input)
      theMap = fromList $ do
        row <- [0 .. (height - 1)]
        column <- [0 .. (width - 1)]
        pure ((column, row), input !! row !! column)
   in FlexGrid 0 0 width height theMap

count :: forall a. Eq a => a -> FlexGrid a -> Int
count v (FlexGrid {theMap}) = length . filter (== v) . fmap snd . toList $ theMap