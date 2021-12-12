module Days.Day12.Cave where

import Data.Char
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lib (Parseable (parse))
import Prelude

data Cave = Start | Big Text | Small Text | End
  deriving (Show, Eq, Ord, Generic)

instance Hashable Cave

instance Parseable Cave where
  parse "start" = Start
  parse "end" = End
  parse a | isUpper (T.head a) = Big a
  parse a = Small a

validTransition :: Cave -> Cave -> Bool
validTransition Start _ = True
validTransition _ Start = False
validTransition _ End = True
validTransition End _ = False
validTransition _ _ = True

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False

isEnd :: Cave -> Bool
isEnd End = True
isEnd _ = False