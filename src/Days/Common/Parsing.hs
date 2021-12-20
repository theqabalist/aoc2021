module Days.Common.Parsing where

import Control.Applicative (some)
import Data.Attoparsec.Text (Parser, digit)
import Data.Char (digitToInt)
import Data.Text (lines, map, singleton, unpack)
import Lib (Parseable (parse))
import Prelude (Int, Show, fmap, read, (.), (<$>))

int :: Parser Int
int = read <$> some digit

newtype DigitLines = DigitLines [[Int]]
  deriving (Show)

instance Parseable DigitLines where
  parse = DigitLines . fmap (fmap digitToInt . unpack) . lines

newtype FlexLines a = FlexLines [[a]]
  deriving (Show)

instance (Parseable a) => Parseable (FlexLines a) where
  parse = FlexLines . fmap (fmap (parse . singleton) . unpack) . lines