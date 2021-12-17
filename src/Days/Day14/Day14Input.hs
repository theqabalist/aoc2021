module Days.Day14.Day14Input where

import Data.Attoparsec.Text (Parser, endOfLine, letter, many1, parseOnly, string)
import Data.Either (fromRight)
import Data.Map (Map, fromList)
import Lib (Parseable (parse))
import Prelude

newtype Day14Input = Day14Input (String, Map String Char)
  deriving (Show)

template :: Parser String
template = many1 letter <* endOfLine

mapping :: Parser ((Char, Char), Char)
mapping = (,) <$> (((,) <$> letter <*> letter) <* string " -> ") <*> (letter <* endOfLine)

day14Input :: Parser Day14Input
day14Input = do
  tmpl <- template <* endOfLine
  mappings <- many1 mapping
  let modified = fromList $ fmap (\((a, b), c) -> ([a, b], c)) mappings
  pure $ Day14Input (tmpl, modified)

instance Parseable Day14Input where
  parse = fromRight (error "Invalid Input") . parseOnly day14Input