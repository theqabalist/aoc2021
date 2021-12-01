{-# LANGUAGE OverloadedStrings #-}

module Days.Day1 (partOne, partTwo) where

import Data.Foldable (foldl')
import Data.Text (Text, lines, unpack)
import Lib (Parseable (parse), aperture)
import Prelude hiding (lines)

newtype Day1Input = Day1Input [Int]

instance Parseable Day1Input where
  parse = Day1Input . fmap (read . unpack) . lines

data ScanState = ScanState Int Int
  deriving (Show)

incs :: ScanState -> Int
incs (ScanState _ x) = x

partOne :: Day1Input -> Int
partOne (Day1Input nums) =
  incs $
    foldl'
      (\(ScanState prev incs) elem -> ScanState elem (if elem > prev then incs + 1 else incs))
      (ScanState (-1) (-1))
      nums

partTwo :: Day1Input -> Int
partTwo (Day1Input nums) = (partOne . Day1Input) $ sum <$> aperture 3 nums