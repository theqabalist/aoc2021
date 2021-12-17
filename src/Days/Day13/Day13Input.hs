module Days.Day13.Day13Input where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, parseOnly, string)
import Data.Either (fromRight)
import Data.HashSet (fromList)
import Days.Common.Parsing (int)
import Days.Day13.CoordBag (CoordBag (..))
import Days.Day13.Fold
import Lib (Parseable (parse))
import Prelude hiding (Left)

newtype Day13Input = Day13Input (CoordBag, [Fold])
  deriving (Show)

point :: Parser (Int, Int)
point = (,) <$> (int <* string ",") <*> int

foldUp :: Parser Fold
foldUp = Up <$> (string "fold along y=" *> int)

foldLeft :: Parser Fold
foldLeft = Left <$> (string "fold along x=" *> int)

fold :: Parser Fold
fold = foldUp <|> foldLeft

day13Input :: Parser Day13Input
day13Input = Day13Input <$> ((,) <$> (CoordBag . fromList <$> many (point <* endOfLine) <* endOfLine) <*> (many (fold <* endOfLine) <* endOfInput))

instance Parseable Day13Input where
  parse = fromRight (error "invalid input") . parseOnly day13Input