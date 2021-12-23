module Days.Common.Vec3 where

import Data.Attoparsec.Text (Parser, decimal, signed, string)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prelude

newtype Vec3 = Vec3 (Int, Int, Int)
  deriving (Show, Eq, Ord, Generic)

instance Hashable Vec3

differential :: Vec3 -> Vec3 -> Vec3
differential (Vec3 (x0, y0, z0)) (Vec3 (x1, y1, z1)) = Vec3 (x1 - x0, y1 - y0, z1 - z0)

translate :: Vec3 -> Vec3 -> Vec3
translate (Vec3 (x0, y0, z0)) (Vec3 (x, y, z)) = Vec3 (x0 + x, y0 + y, z0 + z)

permN :: Int -> Vec3 -> Vec3
permN 0 v = v
permN 1 (Vec3 (x, y, z)) = Vec3 (x, z, y)
permN 2 (Vec3 (x, y, z)) = Vec3 (y, x, z)
permN 3 (Vec3 (x, y, z)) = Vec3 (y, z, x)
permN 4 (Vec3 (x, y, z)) = Vec3 (z, x, y)
permN 5 (Vec3 (x, y, z)) = Vec3 (z, y, x)
permN n v = permN (n `mod` 6) v

toList :: Vec3 -> [Int]
toList (Vec3 (x, y, z)) = [x, y, z]

vec3 :: Parser Vec3
vec3 = Vec3 <$> ((,,) <$> (signed decimal <* string ",") <*> (signed decimal <* string ",") <*> signed decimal)

manhattanDistance :: Vec3 -> Vec3 -> Int
manhattanDistance (Vec3 (x0, y0, z0)) (Vec3 (x1, y1, z1)) = abs (x1 - x0) + abs (y1 - y0) + abs (z1 - z0)