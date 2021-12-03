module Days.Day3.Numeric where

import Data.Vector (Vector, fromList, zipWith)
import Prelude hiding (zipWith)
import Data.Char (digitToInt)

baseLifter :: Int -> Vector Int -> Int
baseLifter base bigEndian =
  let exponents = fromList $ (base ^) <$> (reverse [0 .. (length bigEndian - 1)])
   in sum $ zipWith (*) bigEndian exponents

parseInt :: Int -> String -> Int
parseInt base input = baseLifter base $ fromList $ fmap digitToInt input