module Days.Day24.Machine where

import Prelude

data Register = X | Y | Z | W
  deriving (Show)

data Machine = Machine
  { x :: Int,
    y :: Int,
    z :: Int,
    w :: Int
  }
  deriving (Show)

set :: Register -> Int -> Machine -> Machine
set X x m = m {x = x}
set Y y m = m {y = y}
set Z z m = m {z = z}
set W w m = m {w = w}

read :: Register -> Machine -> Int
read X m = x m
read Y m = y m
read Z m = z m
read W m = w m