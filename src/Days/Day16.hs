module Days.Day16 where

import Control.Monad.State (evalStateT)
import Data.Binary.Get (runGet)
import Data.ByteString.Lazy (ByteString, pack)
import Data.Text (chunksOf, unpack)
import Days.Day16.Packet (getPacket, interpretPacket, totalAllVersions)
import Lib (Parseable (parse))
import Numeric (readHex)
import Prelude

newtype Day16Input = Day16Input {runInput :: ByteString}
  deriving (Show)

instance Parseable Day16Input where
  parse = Day16Input . pack . fmap (fst . head . readHex . unpack) . chunksOf 2

partOne :: Day16Input -> Int
partOne = totalAllVersions . runGet (evalStateT getPacket (0, Nothing)) . runInput

partTwo :: Day16Input -> Integer
partTwo = interpretPacket . runGet (evalStateT getPacket (0, Nothing)) . runInput