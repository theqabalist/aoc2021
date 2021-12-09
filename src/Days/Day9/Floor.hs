{-# LANGUAGE NamedFieldPuns #-}

module Days.Day9.Floor where

import Data.Foldable (foldl')
import qualified Data.HashSet as HS
import Data.Map (Map, fromList, lookup, toList)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow, traceShowId)
import Prelude hiding (lookup)

data Floor = Floor
  { width :: Int,
    height :: Int,
    theMap :: Map (Int, Int) Int
  }
  deriving (Show)

inbounds :: (Int, Int) -> Floor -> Bool
inbounds (x, y) f | x < 0 || y < 0 = False
inbounds (x, y) Floor {width} | x >= width = False
inbounds (x, y) Floor {height} | y >= height = False
inbounds _ _ = True

adjacentLocations :: (Int, Int) -> Floor -> [(Int, Int)]
adjacentLocations (x, y) floor = filter (`inbounds` floor) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

adjacentValues :: (Int, Int) -> Floor -> [Int]
adjacentValues coord floor@Floor {theMap} =
  fromMaybe (error $ "adjacencies to " <> show coord <> " are out of bounds of the map") $
    traverse (`lookup` theMap) $ adjacentLocations coord floor

isLow :: (Int, Int) -> Floor -> Bool
isLow coord floor@Floor {theMap} =
  let asked = fromMaybe (error $ (show coord) <> " is out of bounds of the map") $ lookup coord theMap
      adjacent = adjacentValues coord floor
   in all (> asked) adjacent

lowCoords :: Floor -> [(Int, Int)]
lowCoords floor@(Floor {theMap}) = foldl' (\acc (coord, v) -> acc ++ if isLow coord floor then [coord] else []) [] (toList theMap)

lowValues :: Floor -> [Int]
lowValues floor@(Floor {theMap}) = foldl' (\acc (coord, v) -> acc ++ if isLow coord floor then [v] else []) [] (toList theMap)

valAt :: (Int, Int) -> Floor -> Int
valAt coord (Floor {theMap}) = fromMaybe (error $ (show coord) <> " is out of bounds of the map") $ lookup coord theMap

newtype Basin = Basin [(Int, Int)]
  deriving (Eq, Show)

size :: Basin -> Int
size (Basin b) = length b

instance Ord Basin where
  compare (Basin b1) (Basin b2) = compare (length b1) (length b2)

-- This function requires that the input be a low point
traceBasin :: (Int, Int) -> Floor -> Basin
traceBasin fromLowPoint floor = Basin $ HS.toList $ traceBasin' [fromLowPoint] (HS.fromList []) floor
  where
    traceBasin' [] traced floor = traced
    traceBasin' (coord : horizon) traced floor@(Floor {theMap}) =
      let adjacent = adjacentLocations coord floor
          noNines = filter ((< 9) . (`valAt` floor)) adjacent
          withoutVisited = filter (not . (`HS.member` traced)) noNines
       in traceBasin' (horizon ++ withoutVisited) (HS.insert coord traced) floor

findBasins :: Floor -> [Basin]
findBasins floor = (`traceBasin` floor) <$> lowCoords floor

fromIntArrArr :: [[Int]] -> Floor
fromIntArrArr input =
  let height = length input
      width = length (head input)
      theMap = fromList $ do
        row <- [0 .. (height - 1)]
        column <- [0 .. (width - 1)]
        pure ((column, row), input !! row !! column)
   in Floor width height theMap
