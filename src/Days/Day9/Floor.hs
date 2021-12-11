{-# LANGUAGE NamedFieldPuns #-}

module Days.Day9.Floor where

import Data.Foldable (foldl')
import qualified Data.HashSet as HS
import Data.Map (Map, fromList, lookup, toList)
import Data.Maybe (fromMaybe)
import Days.Common.DigitGrid (DigitGrid (..), adjacentLocations, adjacentValues)
import Debug.Trace (traceShow, traceShowId)
import Prelude hiding (lookup)

type Floor = DigitGrid

isLow :: (Int, Int) -> Floor -> Bool
isLow coord floor@DigitGrid {theMap} =
  let asked = fromMaybe (error $ show coord <> " is out of bounds of the map") $ lookup coord theMap
      adjacent = adjacentValues coord floor
   in all (> asked) adjacent

lowCoords :: Floor -> [(Int, Int)]
lowCoords floor@DigitGrid {theMap} = foldl' (\acc (coord, v) -> acc ++ [coord | isLow coord floor]) [] (toList theMap)

lowValues :: Floor -> [Int]
lowValues floor@DigitGrid {theMap} = foldl' (\acc (coord, v) -> acc ++ [v | isLow coord floor]) [] (toList theMap)

valAt :: (Int, Int) -> Floor -> Int
valAt coord DigitGrid {theMap} = fromMaybe (error $ show coord <> " is out of bounds of the map") $ lookup coord theMap

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
    traceBasin' (coord : horizon) traced floor@DigitGrid {theMap} =
      let adjacent = adjacentLocations coord floor
          noNines = filter ((< 9) . (`valAt` floor)) adjacent
          withoutVisited = filter (not . (`HS.member` traced)) noNines
       in traceBasin' (horizon ++ withoutVisited) (HS.insert coord traced) floor

findBasins :: Floor -> [Basin]
findBasins floor = (`traceBasin` floor) <$> lowCoords floor