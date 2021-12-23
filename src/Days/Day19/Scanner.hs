module Days.Day19.Scanner where

import Control.Applicative
import Data.Attoparsec.Text (Parser, decimal, endOfLine, string)
import Data.Hashable (Hashable)
import qualified Days.Common.Vec3 as V3
import GHC.Generics (Generic)
import Prelude

data Scanner = Scanner Int [V3.Vec3]
  deriving (Show, Eq, Ord, Generic)

instance Hashable Scanner

parseScanner :: Parser Scanner
parseScanner = do
  _ <- string "--- scanner "
  ident <- decimal <* " ---" <* endOfLine
  beacons <- many (V3.vec3 <* endOfLine)
  pure $ Scanner ident beacons

type Orientation = (Int, Int, Int, Int)

reorient :: Orientation -> Orientation -> Orientation
reorient (xrot1, yrot1, zrot1, shift1) (xrot2, yrot2, zrot2, shift2) = (xrot1 * xrot2, yrot1 * yrot2, zrot1 * zrot2, (shift1 + shift2) `mod` 3)

rotations :: Scanner -> [(Orientation, Scanner)]
rotations (Scanner n vecs) = do
  xrot <- [-1, 1]
  yrot <- [-1, 1]
  zrot <- [-1, 1]
  perm <- [0 .. 5]
  pure $ ((xrot, yrot, zrot, perm), Scanner n $ (\(V3.Vec3 (x0, y0, z0)) -> V3.permN perm $ V3.Vec3 (x0 * xrot, y0 * yrot, z0 * zrot)) <$> vecs)

correct :: V3.Vec3 -> Orientation -> Scanner -> Scanner
correct offset (xrot, yrot, zrot, perm) (Scanner n vecs) = Scanner n $ (\(V3.Vec3 (x0, y0, z0)) -> V3.translate (V3.permN perm (V3.Vec3 (x0 * xrot, y0 * yrot, z0 * zrot))) offset) <$> vecs

beaconsInCommon' :: Orientation -> Scanner -> Scanner -> [(V3.Vec3, Orientation, Int)]
beaconsInCommon' _ (Scanner _ []) _ = [(V3.Vec3 (0, 0, 0), (0, 0, 0, 0), 0)]
beaconsInCommon' _ _ (Scanner _ []) = [(V3.Vec3 (0, 0, 0), (0, 0, 0, 0), 0)]
beaconsInCommon' orientation (Scanner _ bs1) (Scanner _ bs2) =
  let allPairs = bs1 >>= (\b1' -> bs2 >>= (\b2' -> pure (b1', b2')))
      offsets = (uncurry (flip V3.differential)) <$> allPairs
      compareds = (\offset -> (offset, (\(b1', b2') -> b1' == V3.translate b2' offset) <$> allPairs)) <$> offsets
   in (\(offset, items) -> (offset, orientation, length $ filter id items)) <$> compareds

beaconsInCommon :: Scanner -> Scanner -> [(Int, (V3.Vec3, Orientation, Int))]
beaconsInCommon s1 s2@(Scanner n _) =
  let rotationsOfS2 = rotations s2
   in rotationsOfS2 >>= (\(orientation, s2') -> (n,) <$> beaconsInCommon' orientation s1 s2')