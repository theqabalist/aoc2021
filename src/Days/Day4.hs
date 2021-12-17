module Days.Day4 where

import Data.Foldable (find, foldl')
import Data.List.Split (chunksOf)
import Data.Text (lines, splitOn, strip, unlines, unpack)
import Days.Day4.BingoBoard (BingoBoard, bingo, call, finalize)
import Lib (Parseable (parse))
import Prelude hiding (lines, unlines)

data Day4Input = Day4Input [Int] [BingoBoard]

instance Parseable Day4Input where
  parse input =
    let split = filter (/= "") $ lines input
        calls = read . unpack <$> splitOn "," (head split) :: [Int]
        boards = parse . unlines <$> chunksOf 5 (strip <$> tail split)
     in Day4Input calls boards

partOne :: Day4Input -> Int
partOne (Day4Input calls boards) =
  let (foundCall, [foundBoard]) =
        foldl'
          ( \(lastCall, bs) c ->
              if length bs == 1
                then (lastCall, bs)
                else
                  ( let called = call c <$> bs
                     in case find bingo called of
                          Just board -> (c, [board])
                          Nothing -> (c, called)
                  )
          )
          (0, boards)
          calls
   in finalize foundCall foundBoard

partTwo :: Day4Input -> Int
partTwo (Day4Input calls boards) =
  let (foundCall, [foundBoard]) =
        foldl'
          ( \(lastCall, bs) c ->
              if length bs == 1 && bingo (head bs)
                then (lastCall, bs)
                else
                  ( let called = call c <$> filter (not . bingo) bs
                     in (c, called)
                  )
          )
          (0, boards)
          calls
   in finalize foundCall foundBoard