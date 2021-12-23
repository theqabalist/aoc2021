module Days.Day22.Activation where

import Data.Attoparsec.Text
import Data.Text (Text)
import Prelude

{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

data Activation = On (Int, Int) (Int, Int) (Int, Int) | Off (Int, Int) (Int, Int) (Int, Int)
  deriving (Show)

parseRange :: Text -> Parser (Int, Int)
parseRange dir = (,) <$> (string (dir <> "=") *> signed decimal) <*> (string ".." *> signed decimal)

parseXYZ :: Parser ((Int, Int), (Int, Int), (Int, Int))
parseXYZ = (,,) <$> (parseRange "x" <* string ",") <*> (parseRange "y" <* string ",") <*> parseRange "z"

parseOn :: Parser Activation
parseOn = string "on " *> (uncurry3 On <$> (parseXYZ <* endOfLine))

parseOff :: Parser Activation
parseOff = string "off " *> (uncurry3 Off <$> (parseXYZ <* endOfLine))

inbounds :: (Int, Int) -> Activation -> Bool
inbounds (low, high) (On (x1, x2) (y1, y2) (z1, z2)) = all (<= high) [x2, y2, z2] && all (>= low) [x1, y1, z1]
inbounds (low, high) (Off (x1, x2) (y1, y2) (z1, z2)) = all (<= high) [x2, y2, z2] && all (>= low) [x1, y1, z1]