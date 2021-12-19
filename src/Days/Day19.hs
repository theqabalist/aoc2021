module Days.Day19 where

import Control.Applicative hiding (empty)
import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, many1, parseOnly)
import Data.Either (fromRight)
import Data.Foldable (Foldable (toList))
import Data.HashSet (HashSet, difference, empty, fromList, size, union)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Days.Day19.Scanner
import qualified Days.Day19.Vec3 as V3
import Debug.Trace
import Lib (Parseable (parse))
import Prelude

newtype Day19Input = Day19Input [Scanner]
  deriving (Show)

instance Parseable Day19Input where
  parse = fromRight (error "invalid input") <$> parseOnly day19Input

day19Input :: Parser Day19Input
day19Input =
  Day19Input <$> do
    scanners <- many1 (parseScanner <* endOfLine)
    endOfInput
    pure scanners

plotScanners :: [Scanner] -> [(Scanner, V3.Vec3, Orientation)]
plotScanners [] = error "invalid input"
plotScanners (s1 : ss) = plotScanners' [] [(s1, V3.Vec3 (0, 0, 0), (1, 1, 1, 0))] (fromList ss)
  where
    plotScanners' finalized searching ss' | size ss' == 0 = finalized ++ searching
    plotScanners' finalized searching ss' =
      let found =
            do
              (prev, offset, orientation) <- searching
              let corrected = correct offset orientation prev
              (ident, (offset', orientation', _)) <- fromJust <$> filter isJust (find ((== 12) . \(_, (_, _, x)) -> x) . beaconsInCommon corrected <$> (traceShow $ size ss') toList ss')
              let scanner = fromJust $ find (\(Scanner n _) -> n == ident) ss'
              pure (scanner, offset', orientation')
          foundScanners = fromList $ (\(s, _, _) -> s) <$> found
       in plotScanners' (finalized ++ searching) (traceShow (length found) found) (ss' `difference` foundScanners)

mergeBeacons :: [(Scanner, V3.Vec3, Orientation)] -> HashSet V3.Vec3
mergeBeacons = foldr combine' empty
  where
    combine' (scanner, offset, orientation) vecs =
      let (Scanner _ vs) = correct offset orientation scanner
       in (vecs `union` fromList vs)

partOne :: Day19Input -> Int
partOne (Day19Input scanners) = size $ mergeBeacons $ plotScanners scanners

furthestDistance :: [V3.Vec3] -> Int
furthestDistance offsets =
  let allPairs = offsets >>= (\s1 -> offsets >>= (\s2 -> if s1 == s2 then [] else pure (s1, s2)))
      distances = uncurry V3.manhattanDistance <$> allPairs
   in foldr max 0 distances

partTwo :: Day19Input -> Int
partTwo (Day19Input scanners) = furthestDistance $ (\(_, offset, _) -> offset) <$> plotScanners scanners