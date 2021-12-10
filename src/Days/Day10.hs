{-# LANGUAGE OverloadedStrings #-}

module Days.Day10 where

import Data.List (find, foldl', sort)
import Data.Maybe (isJust)
import Data.Text (Text, lines, unpack)
import Debug.Trace (traceShow)
import Prelude (Bool (..), Char, Double, Eq ((==)), Int, String, const, error, even, filter, floor, fmap, fromIntegral, fst, head, length, maybe, mod, not, odd, reverse, snd, sum, undefined, (!!), ($), (*), (+), (.), (/), (<$>), (<=))

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _ = 0

inList :: Eq a => a -> [a] -> Bool
inList a as = isJust (find (== a) as)

openingChars :: String
openingChars = "({[<"

closes :: Char -> Char -> Bool
closes '(' ')' = True
closes '[' ']' = True
closes '{' '}' = True
closes '<' '>' = True
closes _ _ = False

closing :: Char -> Char
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'
closing _ = undefined

linePointCollector :: Text -> (String, Int)
linePointCollector input = linePointCollector' [] (unpack input)
  where
    linePointCollector' :: String -> String -> (String, Int)
    linePointCollector' stack (h : t) | h `inList` openingChars = linePointCollector' (h : stack) t
    linePointCollector' (s : stack) (h : t) | closes s h = linePointCollector' stack t
    linePointCollector' (s : stack) (h : t) | not $ closes s h = (s : stack, points h)
    linePointCollector' stack [] = (stack, 0)
    linePointCollector' _ _ = ([], 0)

partOne :: Text -> Int
partOne = sum . fmap (snd . linePointCollector) . lines

points2 :: Char -> Int
points2 ')' = 1
points2 ']' = 2
points2 '}' = 3
points2 '>' = 4
points2 _ = 0

midpoint :: [a] -> a
midpoint [] = error "no midpoint for empty list"
midpoint l | even (length l) = error "no midpoint length % 2"
midpoint l
  | odd (length l) =
    let midpoint = floor $ fromIntegral (length l) / 2.0
     in l !! midpoint
midpoint (a : as) = midpoint (a : as)

partTwo :: Text -> Int
partTwo input =
  let allLines = lines input
      uncorrupted = filter ((<= 0) . snd . linePointCollector) allLines
      scoreClosed :: String -> Int
      scoreClosed closing = foldl' (\score char -> 5 * score + points2 char) 0 closing
      closings = fmap closing . fst . linePointCollector <$> uncorrupted
   in midpoint $ sort $ fmap scoreClosed closings