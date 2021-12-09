{-# LANGUAGE NamedFieldPuns #-}

module Days.Day2.Position where

import Control.Monad.Except (Except, runExcept)
import Data.Either (fromRight)
import Data.Text (Text)
import Lib (Parseable (parse))
import Prelude (Int, Show, (*))

data Position = Position
  { x :: Int,
    z :: Int,
    aim :: Int
  }
  deriving (Show)

finalize :: Position -> Int
finalize Position {x, z} = x * z