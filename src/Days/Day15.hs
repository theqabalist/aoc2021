module Days.Day15 where

import Control.Monad.State (State, evalState, execState, get, put)
import Data.Graph.AStar
import Data.HashSet (HashSet, fromList)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Days.Common.DigitGrid
import Days.Common.Parsing (DigitLines)
import Debug.Trace
import Prelude hiding (lookup)

shortestPath :: DigitGrid -> [((Int, Int), Int)]
shortestPath grid =
  let goal = (width grid - 1, height grid - 1)
      adjacencyLookup = M.fromList $ do
        x <- [0 .. (fst goal)]
        y <- [0 .. (snd goal)]
        pure ((x, y), fromList $ adjacentLocations (x, y) grid)
      adjacencyF p = fromMaybe (error (show p <> "isn't in the grid")) $ M.lookup p adjacencyLookup
      costF p q = valueAt q grid
      heuristicF (x, y) = (fst goal - 1) + (snd goal - y)
      goalF = (== goal)
      start = (0, 0)
      path = aStar adjacencyF costF heuristicF goalF start
   in (\x -> (x, valueAt x grid)) <$> fromJust path

partOne :: DigitLines -> Int
partOne = sum . fmap snd . shortestPath . fromDigitLines

partTwo :: DigitLines -> Int
partTwo =
  sum . fmap snd . shortestPath
    . tileWith
      5
      5
      (\(ix, jy, i, j) kvs -> (\((x, y), v) -> ((x + ix, y + jy), ((v - 1 + i + j) `mod` 9) + 1)) <$> kvs)
    . fromDigitLines