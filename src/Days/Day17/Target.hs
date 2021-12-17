module Days.Day17.Target where

import Data.Attoparsec.Text (Parser, decimal, endOfInput, parseOnly, signed, string)
import Data.Either (fromRight)
import Lib (Parseable (parse))
import Prelude

data Target = Target
  { xmin :: Int,
    xmax :: Int,
    ymin :: Int,
    ymax :: Int
  }
  deriving (Show)

targetParser :: Parser Target
targetParser = Target <$> (string "target area: x=" *> signed decimal <* string "..") <*> (signed decimal <* string ", y=") <*> (signed decimal <* string "..") <*> signed decimal <* endOfInput

overshot :: (Int, Int) -> Target -> Bool
overshot (x, y) Target {xmax, ymin} = x > xmax || y < ymin

within :: (Int, Int) -> Target -> Bool
within (x, y) t@Target {xmin, ymax} = x >= xmin && y <= ymax && not (overshot (x, y) t)

overshotY :: Int -> Target -> Bool
overshotY y Target {ymin} = y < ymin

withinY :: Int -> Target -> Bool
withinY y t@Target {ymax} = y <= ymax && not (overshotY y t)

instance Parseable Target where
  parse = fromRight (error "invalid input") . parseOnly targetParser