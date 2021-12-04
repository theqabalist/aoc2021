{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Days.Day4.BingoBoard where

import Data.Foldable (foldl')
import Data.Map (Map, fromList, toList, lookup, insert)
import Data.Text (lines, splitOn, strip, unpack, replace)
import Data.Tuple (swap)
import Lib (Parseable (parse))
import Prelude hiding (lines, lookup)
import Data.Maybe (fromMaybe)

data BoardLocation = Marked Int | Unmarked Int
  deriving (Show, Eq, Ord)

isMarked :: BoardLocation -> Bool
isMarked (Marked _) = True
isMarked _ = False

unwrap :: BoardLocation -> Int
unwrap (Marked v) = v
unwrap (Unmarked v) = v

data BingoBoard = BingoBoard
  { byLocation :: Map (Int, Int) BoardLocation,
    byNumber :: Map Int (Int, Int),
    bingo :: Bool
  }
  deriving (Show)

column :: Int -> BingoBoard -> [BoardLocation]
column n (BingoBoard {byLocation}) =
  let wholeColumn current depth n = case lookup (depth, n) byLocation of
        Just v -> wholeColumn (current ++ [v]) (depth + 1) n
        Nothing -> current
   in wholeColumn [] 0 n

row :: Int -> BingoBoard -> [BoardLocation]
row n (BingoBoard {byLocation}) =
  let wholeRow current depth n = case lookup (n, depth) byLocation of
        Just v -> wholeRow (current ++ [v]) (depth + 1) n
        Nothing -> current
   in wholeRow [] 0 n

fromListsOfLists :: [[Int]] -> BingoBoard
fromListsOfLists input =
  let numRows = length input
      byNumber =
        foldl'
          ( \coll row ->
              let numColumns = length (input !! row)
                  added = foldl' (\coll2 column -> coll2 ++ [(input !! row !! column, (row, column))]) coll [0 .. (numColumns - 1)]
               in added
          )
          []
          [0 .. (numRows - 1)]
      byLocation = (\(a, b) -> (b, Unmarked a)) <$> byNumber
   in BingoBoard {byLocation = fromList byLocation, byNumber = fromList byNumber, bingo = False}

call :: Int -> BingoBoard -> BingoBoard
call num b@(BingoBoard {byLocation, byNumber}) = fromMaybe b $ do
    (r, c) <- lookup num byNumber
    let newLocations = insert (r, c) (Marked num) byLocation
    let newBoard = b {byLocation = newLocations}
    pure (newBoard {bingo = all isMarked (column c newBoard) || all isMarked (row r newBoard)})

instance Parseable BingoBoard where
  parse = fromListsOfLists . fmap (fmap (read . unpack) . splitOn " " . replace "  " " ") . lines

finalize :: Int -> BingoBoard -> Int
finalize call (BingoBoard {byLocation}) = let
    unmarked = unwrap <$> (filter (\x -> not (isMarked x)) $ snd <$> toList byLocation)
    in call * (sum unmarked)