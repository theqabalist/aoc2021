module Days.Common.Parsing where

import Control.Applicative (some)
import Data.Attoparsec.Text (Parser, digit)
import Data.Char (digitToInt)
import Data.Text (lines, unpack)
import Lib (Parseable (parse))
import Prelude (Int, Show, fmap, read, (.), (<$>))

int :: Parser Int
int = read <$> some digit

newtype DigitLines = DigitLines [[Int]]
  deriving (Show)

instance Parseable DigitLines where
  parse = DigitLines . fmap (fmap digitToInt . unpack) . lines