module Days.Day22 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Either (fromRight)
import Data.Foldable (foldl')
import Days.Day22.Activation
import Days.Day22.Box3 hiding (volume)
import Days.Day22.Expression
import Lib (Parseable (parse))
import Prelude hiding (break)

newtype Day22Input = Day22Input {day22 :: [Activation]}
  deriving (Show)

instance Parseable Day22Input where
  parse = Day22Input . fromRight (error "invalid input") . parseOnly (many (parseOn <|> parseOff) <* endOfInput)

activationToBoxInstruction :: Activation -> (Expression -> Expression)
activationToBoxInstruction (On (x1, x2) (y1, y2) (z1, z2)) = (`Union` (Base $ mkBox (x1, y1, z1) (x2 + 1, y2 + 1, z2 + 1)))
activationToBoxInstruction (Off (x1, x2) (y1, y2) (z1, z2)) = (`Difference` (Base $ mkBox (x1, y1, z1) (x2 + 1, y2 + 1, z2 + 1)))

partGeneric :: (Activation -> Bool) -> Day22Input -> Int
partGeneric f = volume . foldl' (flip ($)) (Base nullBox) . fmap activationToBoxInstruction . filter f . day22

partOne :: Day22Input -> Int
partOne = partGeneric (inbounds (-50, 50))

partTwo :: Day22Input -> Int
partTwo = partGeneric (const True)