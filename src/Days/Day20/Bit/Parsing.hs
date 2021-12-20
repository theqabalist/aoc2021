module Days.Day20.Bit.Parsing where

import Control.Applicative
import Data.Attoparsec.Text (Parser, string)
import Data.Functor (($>))
import Days.Day20.Bit

bit :: Parser Bit
bit = string "#" $> One <|> string "." $> Zero