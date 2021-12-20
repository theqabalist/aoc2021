module Days.Day20.Bit where

import Data.Monoid
import Prelude

data Bit = One | Zero
  deriving (Eq)

instance Show Bit where
  show One = "#"
  show Zero = "."

instance Semigroup Bit where
  Zero <> Zero = Zero
  Zero <> One = One
  One <> Zero = One
  One <> One = One

instance Monoid Bit where
  mempty = Zero

toInt :: Bit -> Int
toInt One = 1
toInt Zero = 0