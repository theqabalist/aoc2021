{-# LANGUAGE OverloadedStrings #-}

module Days.Day9 where

import Data.Char (digitToInt)
import Data.List (sort)
import Data.Text (Text, lines, unpack)
import Days.Day9.Floor
import Lib (Parseable (parse))
import Prelude (Int, fmap, product, reverse, sum, take, ($), (+), (.), (<$>))

newtype Day9Input = Day9Input [[Int]]

instance Parseable Day9Input where
  parse = Day9Input . fmap (fmap digitToInt . unpack) . lines

partOne :: Day9Input -> Int
partOne (Day9Input input) = sum ((+ 1) <$> lowValues (fromIntArrArr input))

partTwo :: Day9Input -> Int
partTwo (Day9Input input) =
  let floor = fromIntArrArr input
      basins = findBasins floor
   in product $ take 3 $ reverse $ size <$> sort basins