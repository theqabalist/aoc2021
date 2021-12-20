module Days.Day3 where

import Data.Char (intToDigit)
import Data.Text (Text, lines, unpack)
import Days.Common.Numeric (parseInt)
import Days.Day3.RateCollector (epsilon, fromSignals, gamma, leastCommon, mostCommon)
import Prelude hiding (lines)

partOne :: Text -> Int
partOne input =
  let signals = unpack <$> lines input
      processed = fromSignals signals
   in gamma processed * epsilon processed

refineOxygen :: Int -> [String] -> [String]
refineOxygen pos signals | pos >= length (head signals) = signals
refineOxygen _ signals | length signals == 1 = signals
refineOxygen pos signals =
  let processed = fromSignals signals
      mc = mostCommon 1 pos processed
   in refineOxygen (pos + 1) $ filter (\s -> s !! pos == intToDigit mc) signals

refineCO2 :: Int -> [String] -> [String]
refineCO2 pos signals | pos >= length (head signals) = signals
refineCO2 _ signals | length signals == 1 = signals
refineCO2 pos signals =
  let processed = fromSignals signals
      mc = leastCommon 0 pos processed
   in refineCO2 (pos + 1) $ filter (\s -> s !! pos == intToDigit mc) signals

partTwo :: Text -> Int
partTwo input =
  let signals = unpack <$> lines input
      oxygen = parseInt 2 $ head $ refineOxygen 0 signals
      co2 = parseInt 2 $ head $ refineCO2 0 signals
   in oxygen * co2