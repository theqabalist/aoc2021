module Days.Day20 where

import Control.Applicative
import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, parseOnly)
import Data.Either (fromRight)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import Days.Common.FlexGrid (FlexGrid (..), convolute', count, depad, fromLines, padRing', valueAt')
import Days.Common.Numeric (baseLifter)
import Days.Common.Parsing (FlexLines (FlexLines))
import Days.Day20.Bit
import Days.Day20.Bit.Parsing
import Lib (Parseable (parse))
import Prelude

data Day20Input = Day20Input [Bit] (FlexGrid Bit)
  deriving (Show)

bitGrid :: Parser (FlexGrid Bit)
bitGrid = fromLines . FlexLines <$> many (many bit <* endOfLine)

day20Input :: Parser Day20Input
day20Input = Day20Input <$> (many bit <* endOfLine <* endOfLine) <*> bitGrid <* endOfInput

instance Parseable Day20Input where
  parse = fromRight (error "invalid input") . parseOnly day20Input

showGrid :: FlexGrid Bit -> String
showGrid g@(FlexGrid minX minY w h _) =
  let rows = chunksOf w $ do
        y0 <- [minY .. minY + h - 1]
        x0 <- [minX .. minX + w - 1]
        pure $ head . show $ valueAt' (x0, y0) g
   in unlines rows

partGeneric :: Int -> Day20Input -> Int
partGeneric depth (Day20Input bitmap grid) =
  let convolution :: [Bit] -> Bit
      convolution as =
        let idx = (baseLifter 2 . V.fromList) $ toInt <$> as
         in bitmap !! idx
      step g = convolute' (3, 3) convolution g
      expanded = (iterate step (iterate padRing' grid !! (depth * 2)) !! depth)
      cleaned = iterate depad expanded !! (depth - 1)
   in count One cleaned

partOne :: Day20Input -> Int
partOne = partGeneric 2

partTwo :: Day20Input -> Int
partTwo = partGeneric 50