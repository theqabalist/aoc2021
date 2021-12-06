{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Days.Day6 where

import Data.Map (Map, empty, insert, lookup, union)
import Data.Text (Text, splitOn, unpack)
import Debug.Trace (traceShowId)
import Lib (Parseable (parse))
import Prelude hiding (lookup)

newtype Day6Input = Day6Input [Int]

instance Parseable Day6Input where
  parse = Day6Input . fmap (read . unpack) . splitOn ","

data Fish = Old Int | New Int
  deriving (Show)

newtype Lifetime = Lifetime {runLifetime :: [(Fish, Int)]}
  deriving (Show)

live :: Int -> Fish -> Lifetime
live days (Old state) =
  let firstspawn = state + 1
      regularspawn = 7 + firstspawn
   in Lifetime ((New 8,) . (days -) <$> [firstspawn, regularspawn .. days])
live days (New state) =
  Lifetime (if days <= 8 then [] else (New 8,) . ((days - 9) -) <$> [0, 7 .. (days -9)])

allTheWorldsChildren :: Int -> Fish -> Int
allTheWorldsChildren days fish = snd $ allTheWorldsChildren' empty days fish
  where
    allTheWorldsChildren' :: Map Int Int -> Int -> Fish -> (Map Int Int, Int)
    allTheWorldsChildren' newCache days fish =
      foldr
        ( \(f, life) (cache, sum) ->
            let found = lookup life cache
             in case found of
                  Just v -> (cache, sum + v)
                  Nothing ->
                    let (underCache, total) = allTheWorldsChildren' cache life f
                        updatedCache = union underCache $ insert life total cache
                     in (updatedCache, sum + total)
        )
        (newCache, 1)
        (runLifetime $ live days fish)

partOne :: Day6Input -> Int
partOne (Day6Input input) = sum $ allTheWorldsChildren 80 . Old <$> input

partTwo :: Day6Input -> Int
partTwo (Day6Input input) = sum $ allTheWorldsChildren 256 . Old <$> input