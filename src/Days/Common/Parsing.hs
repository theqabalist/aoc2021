module Days.Common.Parsing where

import Control.Applicative (some)
import Data.Attoparsec.Text (Parser, digit)
import Prelude (Int, read, (<$>))

int :: Parser Int
int = read <$> some digit