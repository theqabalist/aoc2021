{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Days.Day5.Segment where

import Data.Attoparsec.Text (Parser, endOfLine, skipMany1)
import Data.HashSet (HashSet, fromList, intersection)
import Days.Common.Parsing (int)
import Prelude hiding (length)

data Segment = Segment
  { from :: (Int, Int),
    to :: (Int, Int)
  }
  deriving (Eq, Show)

horizontal :: Segment -> Bool
horizontal (Segment (x1, _) (x2, _)) = x1 == x2

vertical :: Segment -> Bool
vertical (Segment (_, y1) (_, y2)) = y1 == y2

range :: Int -> Int -> [Int]
range v1 v2 | v1 == v2 = [v1]
range v1 v2 | v1 > v2 = reverse [v2 .. v1]
range v1 v2 = [v1 .. v2]

mkBag :: Segment -> HashSet (Int, Int)
mkBag (Segment (x1, y1) (x2, y2)) | x1 == x2 = fromList $ (x1,) <$> range y1 y2
mkBag (Segment (x1, y1) (x2, y2)) | y1 == y2 = fromList $ (,y1) <$> range x1 x2
mkBag (Segment (x1, y1) (x2, y2)) = fromList $ zip (range x1 x2) (range y1 y2)

diagonal :: Segment -> Bool
diagonal (Segment (x1, y1) (x2, y2)) | x1 == x2 || y1 == y2 = False
diagonal _ = True

onSegment :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
onSegment (px, py) (qx, qy) (rx, ry) = qx <= max px rx && qx >= min px rx && qy <= max py ry && qy >= min py ry

orientation :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
orientation (px, py) (qx, qy) (rx, ry) =
  let val = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)
   in if val == 0 then 0 else if val > 0 then 1 else 2

doIntersect :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
doIntersect p1 q1 p2 q2 =
  let o1 = orientation p1 q1 p2
      o2 = orientation p1 q1 q2
      o3 = orientation p2 q2 p1
      o4 = orientation p2 q2 q1
   in (o1 /= o2 && o3 /= o4)
        || (o1 == 0 && onSegment p1 p2 q1)
        || (o2 == 0 && onSegment p1 q2 q1)
        || (o3 == 0 && onSegment p2 p1 q2)
        || (o4 == 0 && onSegment p2 q1 q2)

intersects :: Segment -> Segment -> Bool
intersects (Segment from1 to1) (Segment from2 to2) = doIntersect from1 to1 from2 to2

collinearIntersection :: Segment -> Segment -> HashSet (Int, Int)
collinearIntersection s1 s2 = intersection (mkBag s1) (mkBag s2)

collide :: Segment -> Segment -> HashSet (Int, Int)
collide s1 s2 = intersection (mkBag s1) (mkBag s2)

parsePoint :: Parser (Int, Int)
parsePoint = (,) <$> (int <* skipMany1 ",") <*> int

parseSegment :: Parser Segment
parseSegment = Segment <$> (parsePoint <* skipMany1 " -> ") <*> parsePoint <* endOfLine