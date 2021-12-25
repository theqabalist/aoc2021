module Days.Day25 where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Map (Map, insert, lookup, toList)
import Data.Maybe (fromJust)
import Days.Common.FlexGrid (FlexGrid (..), fromLines)
import Days.Common.Parsing (FlexLines)
import Debug.Trace
import Lib (Parseable (parse))
import Prelude hiding (lookup)

data Floor = East | South | Empty
  deriving (Eq, Ord, Show)

instance Parseable Floor where
  parse ">" = East
  parse "v" = South
  parse "." = Empty
  parse x = error ("unknown floor value '" ++ show x ++ "'")

lookup' :: forall k v. Ord k => k -> Map k v -> v
lookup' k m = fromJust $ lookup k m

moveEast :: FlexGrid Floor -> FlexGrid Floor
moveEast g@FlexGrid {width, theMap} =
  let easts = filter ((== East) . snd) $ toList theMap
      moves = (\((x, y), east) -> (((x, y), east), (((x + 1) `mod` width, y), east))) <$> easts
      valid = filter ((== Empty) . (`lookup'` theMap) . fst . snd) moves
   in g {theMap = foldr (\((fr, _), (to, _)) newMap -> insert to East $ insert fr Empty newMap) theMap valid}

moveSouth :: FlexGrid Floor -> FlexGrid Floor
moveSouth g@FlexGrid {height, theMap} =
  let souths = filter ((== South) . snd) $ toList theMap
      moves = (\((x, y), south) -> (((x, y), south), ((x, (y + 1) `mod` height), south))) <$> souths
      valid = filter ((== Empty) . (`lookup'` theMap) . fst . snd) moves
   in g {theMap = foldr (\((fr, _), (to, _)) newMap -> insert to South $ insert fr Empty newMap) theMap valid}

moveTotal :: FlexGrid Floor -> FlexGrid Floor
moveTotal = moveSouth . moveEast

toChar :: Floor -> Char
toChar East = '>'
toChar South = 'v'
toChar Empty = '.'

visualize :: FlexGrid Floor -> String
visualize FlexGrid {height, theMap} =
  let rows = transpose $ chunksOf height $ toChar . snd <$> toList theMap
   in unlines rows

traceViz :: FlexGrid Floor -> FlexGrid Floor
traceViz x = trace (visualize x) x

partOne :: FlexLines Floor -> Int
partOne input =
  let moves = iterate moveTotal $ fromLines input
      moves' = drop 1 moves
      pairs = zip moves moves'
   in 1 + length (takeWhile (uncurry (/=)) pairs)

partTwo :: FlexLines Floor -> Int
partTwo = const 5