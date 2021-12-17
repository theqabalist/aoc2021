module Days.Day13.CoordBag where

import Data.Foldable (foldl')
import Data.HashSet (HashSet, filter, fromList, map, member, size, toList, union)
import Data.Text (Text, pack, unlines)
import Days.Day13.Fold
import Prelude hiding (Left, filter, map, unlines)

newtype CoordBag = CoordBag (HashSet (Int, Int))
  deriving (Show)

fold :: Fold -> CoordBag -> CoordBag
fold (Up u) (CoordBag bag) =
  let (lowerSection, upperSection) = (filter (\(_, y) -> y <= u) bag, filter (\(_, y) -> y > u) bag)
   in CoordBag $ lowerSection `union` map (\(x, y) -> (x, u - (y - u))) upperSection
fold (Left l) (CoordBag bag) =
  let (leftSection, rightSection) = (filter (\(x, _) -> x <= l) bag, filter (\(x, _) -> x > l) bag)
   in CoordBag $ leftSection `union` map (\(x, y) -> (l - (x - l), y)) rightSection

visibleDots :: CoordBag -> Int
visibleDots (CoordBag bag) = size bag

decodeLetter :: HashSet (Int, Int) -> Char
decodeLetter x | x == fromList (((0,) <$> [0 .. 5]) ++ [(1, 0), (1, 3), (2, 0), (2, 3), (2, 4), (3, 1), (3, 2), (3, 5)]) = 'R'
decodeLetter x | x == fromList (((0,) <$> [1 .. 4]) ++ [(1, 0), (1, 5), (2, 0), (2, 5), (3, 1), (3, 4)]) = 'C'
decodeLetter x | x == fromList (((0,) <$> [0 .. 5]) ++ [(1, 0), (1, 3), (2, 0), (2, 3), (3, 1), (3, 2)]) = 'P'
decodeLetter x | x == fromList (((0,) <$> [0 .. 5]) ++ ((,5) <$> [0 .. 3])) = 'L'
decodeLetter x | x == fromList (((0,) <$> [1 .. 5]) ++ ((3,) <$> [1 .. 5]) ++ [(1, 0), (1, 3), (2, 0), (2, 3)]) = 'A'
decodeLetter x | x == fromList (((0,) <$> [0 .. 5]) ++ [(1, 2), (2, 1), (2, 3), (2, 4), (3, 0), (3, 5)]) = 'K'
decodeLetter x | x == fromList (((0,) <$> [0 .. 5]) ++ ((3,) <$> [0 .. 5]) ++ ((,2) <$> [0 .. 3])) = 'H'
decodeLetter _ = error "unrecognized character"

decodeBag :: CoordBag -> String
decodeBag (CoordBag bag) | size bag == 0 = []
decodeBag (CoordBag bag) =
  let first = filter (\(x, _) -> x <= 4) bag -- every character is 4 wide with a vertical space
      rest = map (\(x, y) -> (x - 5, y)) $ filter (\(x, _) -> x >= 5) bag
   in decodeLetter first : decodeBag (CoordBag rest)

toVisible :: CoordBag -> Text
toVisible (CoordBag bag) =
  let asList = toList bag
      maxX = foldr (max . fst) (-1) asList
      maxY = foldr (max . snd) (-1) asList
   in unlines $ foldl' (\ts y -> ts ++ [pack $ foldl' (\row x -> row ++ [if member (x, y) bag then '#' else '.']) [] [0 .. maxX]]) [] [0 .. maxY]