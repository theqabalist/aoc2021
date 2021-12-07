{-# LANGUAGE OverloadedStrings #-}

module Days.Day7 where

import Data.HashSet (HashSet, fromList, toList)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text, splitOn, strip, unpack)
import Lib (Parseable (parse))

newtype Day7Input = Day7Input (NEL.NonEmpty Int)

instance Parseable Day7Input where
  parse = Day7Input . fmap (read . unpack) . NEL.fromList . splitOn "," . strip

nelMin :: Ord a => NonEmpty a -> a
nelMin (a :| []) = a
nelMin (a :| as) = min a (nelMin (NEL.fromList as))

genericDay7 :: (Int -> Rational) -> Day7Input -> Int
genericDay7 f (Day7Input input) =
  let candidates = fromList (NEL.toList input)
      fuels = fmap (\candidate -> sum $ fmap (f . abs . (candidate -)) input) (NEL.fromList $ toList candidates)
   in floor $ nelMin fuels

partOne :: Day7Input -> Int
partOne = genericDay7 fromIntegral

sumSelf :: Int -> Rational
sumSelf n = fromIntegral (n * (n + 1)) / 2

partTwo :: Day7Input -> Int
partTwo = genericDay7 sumSelf