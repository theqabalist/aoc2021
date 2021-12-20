{-# LANGUAGE NamedFieldPuns #-}

module Days.Day3.RateCollector where

import Data.Vector (Vector, fromList, replicate, zipWith, (!))
import Days.Common.Numeric (baseLifter)
import Prelude hiding (head, replicate, zipWith)
import qualified Prelude as P

data Signal = Signal
  { ones :: Int,
    zeroes :: Int
  }
  deriving (Show)

equal :: Signal -> Bool
equal Signal {ones, zeroes} = ones == zeroes

greater :: Signal -> Int
greater Signal {ones, zeroes} = if ones > zeroes then 1 else 0

lesser :: Signal -> Int
lesser Signal {ones, zeroes} = if ones > zeroes then 0 else 1

newtype RateCollector = RateCollector (Vector Signal)
  deriving (Show)

mkCollector :: Int -> RateCollector
mkCollector n = RateCollector $ replicate n (Signal 0 0)

updateWithString :: String -> RateCollector -> RateCollector
updateWithString input (RateCollector collected) =
  RateCollector $
    zipWith
      (\char s@Signal {ones, zeroes} -> if char == '1' then s {ones = ones + 1} else s {zeroes = zeroes + 1})
      (fromList input)
      collected

gamma :: RateCollector -> Int
gamma (RateCollector signals) = baseLifter 2 $ greater <$> signals

epsilon :: RateCollector -> Int
epsilon (RateCollector signals) = baseLifter 2 $ lesser <$> signals

fromSignals :: [String] -> RateCollector
fromSignals signals =
  let testLength = length (P.head signals)
      collector = mkCollector testLength
   in foldr updateWithString collector signals

mostCommon :: Int -> Int -> RateCollector -> Int
mostCommon ifEqual pos (RateCollector coll) =
  let next = coll ! pos
   in if equal next then ifEqual else greater next

leastCommon :: Int -> Int -> RateCollector -> Int
leastCommon ifEqual pos (RateCollector coll) =
  let next = coll ! pos
   in if equal next then ifEqual else lesser next