module Days.Day12 where

import Days.Day12.CaveSystem
import Prelude hiding (lines)

partOne :: CaveSystem -> Int
partOne = length . explore1

partTwo :: CaveSystem -> Int
partTwo = length . explore2