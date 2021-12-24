module Days.Day24 where

import Control.Monad.Except
import Data.Text hiding (filter)
import Days.Day24.Instruction
import Days.Day24.Machine
import Days.Day24.Parsing
import Debug.Trace
import Prelude

success :: Either String Machine -> Bool
success (Left _) = False
success (Right Machine {z}) = z == 0

findHighest :: (Int -> Bool) -> Int -> Int
findHighest test upper = if test upper then upper else findHighest test (upper - 1)

partOne :: Day24Input -> Either String Machine
partOne (Day24Input instructions) = runExcept . runMachine instructions . (show :: Integer -> String) $ 92928914999991

partTwo :: Day24Input -> Either String Machine
partTwo (Day24Input instructions) = runExcept . runMachine instructions . (show :: Integer -> String) $ 91811211611981