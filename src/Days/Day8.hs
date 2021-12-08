{-# LANGUAGE OverloadedStrings #-}

module Days.Day8 where

import Control.Monad.State (evalState)
import qualified Data.HashSet as HS
import Data.List (find, partition)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, length, lines, splitOn, unpack)
import Days.Day8.CharMapping
  ( CharMapping,
    descramble,
    mkMapping,
    scramble,
    update,
  )
import Days.Day8.Decode
import Debug.Trace (traceShow, traceShowId)
import Lib (Parseable (parse))
import Prelude hiding (length, lines)
import qualified Prelude as P

toTupleUnsafe :: [a] -> (a, a)
toTupleUnsafe [a, b] = (a, b)

newtype Day8Input = Day8Input [([Text], [Text])]

instance Parseable Day8Input where
  parse = Day8Input . fmap (toTupleUnsafe . fmap (splitOn " ") . splitOn " | ") . lines

partOne :: Day8Input -> Int
partOne (Day8Input input) = sum $ P.length . filter (\x -> length x `elem` [2, 3, 4, 7]) . snd <$> input

canonSegmentToDigit :: HS.HashSet Char -> Char
canonSegmentToDigit s | s == HS.fromList "abcefg" = '0'
canonSegmentToDigit s | s == HS.fromList "cf" = '1'
canonSegmentToDigit s | s == HS.fromList "acdeg" = '2'
canonSegmentToDigit s | s == HS.fromList "acdfg" = '3'
canonSegmentToDigit s | s == HS.fromList "bcdf" = '4'
canonSegmentToDigit s | s == HS.fromList "abdfg" = '5'
canonSegmentToDigit s | s == HS.fromList "abdefg" = '6'
canonSegmentToDigit s | s == HS.fromList "acf" = '7'
canonSegmentToDigit s | s == HS.fromList "abcdefg" = '8'
canonSegmentToDigit s | s == HS.fromList "abcdfg" = '9'
canonSegmentToDigit _ = undefined

deduceSegments :: [Text] -> CharMapping
deduceSegments scrambled =
  evalState
    ( decodeEight
        >> decodeOne
        >> decodeSeven
        >> determineA
        >> decodeFour
        >> decodeNine
        >> determineG
        >> determineE
        >> decodeThree
        >> determineD
        >> decodeZero
        >> determineB
        >> determineLibE
        >> decodeSix
        >> determineF
        >> decodeFive
        >> decodeTwo
        >> determineC
        >> finalize
    )
    (scrambled, M.empty, M.empty, mkMapping)

decodeOutputs :: ([Text], [Text]) -> Int
decodeOutputs (signals, outputs) =
  let mapping = deduceSegments signals
      corrected = maybe undefined (fmap canonSegmentToDigit) (sequence $ fmap HS.fromList . mapM (`descramble` mapping) . unpack <$> outputs)
      fromDigits = read corrected
   in fromDigits

partTwo :: Day8Input -> Int
partTwo (Day8Input input) = sum $ decodeOutputs <$> input