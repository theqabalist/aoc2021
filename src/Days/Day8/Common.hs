module Days.Day8.Common where

import Data.HashSet (HashSet, fromList, toList)
import Data.Text (Text, unpack)

toBag :: Text -> HashSet Char
toBag = fromList . unpack

only :: HashSet a -> a
only = head . toList