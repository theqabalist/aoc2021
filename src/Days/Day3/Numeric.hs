module Days.Day3.Numeric where

import Data.Char (digitToInt)
import Data.Vector (Vector, fromList, zipWith)
import Prelude (Int, String, fmap, length, reverse, sum, ($), (*), (-), (<$>), (^))

baseLifter :: Int -> Vector Int -> Int
baseLifter base bigEndian =
  let exponents = fromList $ (base ^) <$> reverse [0 .. (length bigEndian - 1)]
   in sum $ zipWith (*) bigEndian exponents

parseInt :: Int -> String -> Int
parseInt base input = baseLifter base $ fromList $ fmap digitToInt input