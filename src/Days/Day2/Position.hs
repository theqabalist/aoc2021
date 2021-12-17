{-# LANGUAGE NamedFieldPuns #-}

module Days.Day2.Position where

import Prelude (Int, Show, (*))

data Position = Position
  { x :: Int,
    z :: Int,
    aim :: Int
  }
  deriving (Show)

finalize :: Position -> Int
finalize Position {x, z} = x * z