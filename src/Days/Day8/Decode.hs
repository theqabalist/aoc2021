{-# LANGUAGE OverloadedStrings #-}

module Days.Day8.Decode where

import Control.Monad.State (State, get, gets, put)
import Data.HashSet (HashSet, difference, empty, fromList, singleton, size, union)
import Data.List (find, partition)
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text, length)
import Days.Day8.CharMapping (CharMapping, scramble, update)
import Days.Day8.Common (only, toBag)
import Prelude hiding (length, lookup)

type Decoding = State ([Text], Map String (HashSet Char), Map Int (HashSet Char), CharMapping)

requiresNum :: Text -> Int -> Decoding (HashSet Char)
requiresNum what num = do
  (_, _, nums, _) <- get
  let found = lookup num nums
  pure $ fromMaybe (determinationError what num) found

requiresLib :: Text -> String -> Decoding (HashSet Char)
requiresLib what which = do
  (_, lib, _, _) <- get
  let found = lookup which lib
  pure $ fromMaybe (determinationError what which) found

requiresScrambled :: Text -> Char -> Decoding Char
requiresScrambled what letter = do
  (_, _, _, mapping) <- get
  let found = scramble letter mapping
  pure $ fromMaybe (determinationError what letter) found

decodeGeneral :: Int -> (Text -> Bool) -> Decoding ()
decodeGeneral indicated p = do
  (scrambled, lib, nums, mapping) <- get
  let ([raw], without) = partition p scrambled
  let deduced = toBag raw
  put (without, lib, insert indicated deduced nums, mapping)

decodeEight :: Decoding ()
decodeEight = decodeGeneral 8 ((== 7) . length)

decodeOne :: Decoding ()
decodeOne = decodeGeneral 1 ((== 2) . length)

decodeSeven :: Decoding ()
decodeSeven = decodeGeneral 7 ((== 3) . length)

determinationError :: (Show a, Show b) => a -> b -> c
determinationError a b = error $ "tried to determine '" <> show a <> "' without " <> show b

determineA :: Decoding ()
determineA = do
  (scrambled, lib, nums, mapping) <- get
  seven <- requiresNum "a" 7
  one <- requiresNum "a" 1
  let delta = only $ difference seven one
  put (scrambled, lib, nums, update delta 'a' mapping)

determineG :: Decoding ()
determineG = do
  (scrambled, lib, nums, mapping) <- get
  nine <- requiresNum "g" 9
  fourWithTop <- requiresLib "g" "fourWithTop"
  let delta = only $ difference nine fourWithTop
  put (scrambled, lib, nums, update delta 'g' mapping)

decodeFour :: Decoding ()
decodeFour = decodeGeneral 4 ((== 4) . length)

decodeNine :: Decoding ()
decodeNine = do
  (scrambled, lib, nums, mapping) <- get
  four <- requiresNum "9" 4
  a <- requiresScrambled "9" 'a'
  let fourWithTop = four `union` singleton a
  let ([nineRaw], withoutNine) = partition (\x -> length x == 6 && size (difference (toBag x) fourWithTop) == 1) scrambled
  let deducedNine = toBag nineRaw
  put (withoutNine, insert "fourWithTop" fourWithTop lib, insert 9 deducedNine nums, mapping)

determineE :: Decoding ()
determineE = do
  (scrambled, lib, nums, mapping) <- get
  let zeroOrSix = toBag $ fromMaybe (error "tried to determine 'e' with insufficient figures") $ find ((== 6) . length) scrambled
  nine <- requiresNum "e" 9
  let delta = only $ difference zeroOrSix nine
  put (scrambled, lib, nums, update delta 'e' mapping)

decodeThree :: Decoding ()
decodeThree = do
  (scrambled, lib, nums, mapping) <- get
  a <- requiresScrambled "3" 'a'
  g <- requiresScrambled "3" 'g'
  one <- requiresNum "3" 1
  let backwardsC = foldr union empty [singleton a, singleton g, one]
  let ([threeRaw], withoutThree) = partition (\x -> length x == 5 && size (difference (toBag x) backwardsC) == 1) scrambled
  let deducedThree = toBag threeRaw
  put (withoutThree, insert "backwardsC" backwardsC lib, insert 3 deducedThree nums, mapping)

determineD :: Decoding ()
determineD = do
  (scrambled, lib, nums, mapping) <- get
  three <- requiresNum "d" 3
  backwardsC <- requiresLib "d" "backwardsC"
  let delta = only $ difference three backwardsC
  put (scrambled, lib, nums, update delta 'd' mapping)

decodeZero :: Decoding ()
decodeZero = do
  (scrambled, lib, nums, mapping) <- get
  eight <- requiresNum "0" 8
  d <- requiresScrambled "0" 'd'
  let zero = difference eight (singleton d)
  let withoutZero = filter ((/= zero) . toBag) scrambled
  put (withoutZero, lib, insert 0 zero nums, mapping)

determineB :: Decoding ()
determineB = do
  (scrambled, lib, nums, mapping) <- get
  e <- requiresScrambled "b" 'e'
  backwardsC <- requiresLib "b" "backwardsC"
  zero <- requiresNum "b" 0
  let delta = only $ difference zero (foldr union empty [singleton e, backwardsC])
  put (scrambled, lib, nums, update delta 'b' mapping)

determineLibE :: Decoding ()
determineLibE = do
  (scrambled, lib, nums, mapping) <- get
  a <- requiresScrambled "letterE" 'a'
  b <- requiresScrambled "letterE" 'b'
  d <- requiresScrambled "letterE" 'd'
  e <- requiresScrambled "letterE" 'e'
  g <- requiresScrambled "letterE" 'g'
  let letterE = fromList [a, b, d, e, g]
  put (scrambled, insert "letterE" letterE lib, nums, mapping)

decodeSix :: Decoding ()
decodeSix = do
  (scrambled, lib, nums, mapping) <- get
  let ([sixRaw], without) = partition (\x -> length x == 6) scrambled
  let six = toBag sixRaw
  put (without, lib, insert 6 six nums, mapping)

determineF :: Decoding ()
determineF = do
  (scrambled, lib, nums, mapping) <- get
  six <- requiresNum "f" 6
  letterE <- requiresLib "f" "letterE"
  let delta = only $ difference six letterE
  put (scrambled, lib, nums, update delta 'f' mapping)

decodeFive :: Decoding ()
decodeFive = do
  (scrambled, lib, nums, mapping) <- get
  a <- requiresScrambled "5" 'a'
  b <- requiresScrambled "5" 'b'
  d <- requiresScrambled "5" 'd'
  f <- requiresScrambled "5" 'f'
  g <- requiresScrambled "5" 'g'
  let five = fromList [a, b, d, f, g]
  put (scrambled, lib, insert 5 five nums, mapping)

decodeTwo :: Decoding ()
decodeTwo = do
  (scrambled, lib, nums, mapping) <- get
  five <- requiresNum "2" 5
  let two = toBag $ head $ filter ((/= five) . toBag) scrambled
  put (scrambled, lib, insert 2 two nums, mapping)

determineC :: Decoding ()
determineC = do
  (scrambled, lib, nums, mapping) <- get
  two <- requiresNum "c" 2
  letterE <- requiresLib "c" "letterE"
  let delta = only $ difference two letterE
  put (scrambled, lib, nums, update delta 'c' mapping)

finalize :: Decoding CharMapping
finalize = gets (\(_, _, _, mappings) -> mappings)