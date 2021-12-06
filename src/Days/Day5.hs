{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Days.Day5 where

import Data.Attoparsec.Text (endOfInput, many', parseOnly)
import Data.Either (fromRight)
import Data.HashSet (HashSet, empty, size, union, unions)
import Data.Text (Text)
import Days.Day5.Segment (Segment, collide, diagonal, horizontal, intersects, parseSegment, vertical)
import Lib (Parseable (parse))

newtype Day5Input = Day5Input [Segment]
  deriving (Show)

instance Parseable Day5Input where
  parse = Day5Input . fromRight [] . parseOnly (many' parseSegment <* endOfInput)

tallyCollisions :: [Segment] -> HashSet (Int, Int)
tallyCollisions = tallyCollisions' empty
  where
    tallyCollisions' !collected (first : rest) =
      let inters = filter (intersects first) rest
          newCollisions = unions $ map (collide first) inters
          more = collected `union` newCollisions
       in tallyCollisions' more rest
    tallyCollisions' collected [] = collected

partOne :: Day5Input -> Int
partOne (Day5Input segments) = size $ tallyCollisions $ filter (not . diagonal) segments

partTwo :: Day5Input -> Int
partTwo (Day5Input segments) = size $ tallyCollisions segments