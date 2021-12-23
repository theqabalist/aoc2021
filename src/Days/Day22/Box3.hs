module Days.Day22.Box3
  ( Box3,
    mkBox,
    intersect,
    volume,
    null,
    nullBox,
  )
where

import Days.Common.Vec3
import Prelude hiding (null)

data Box3 = Box3
  { from :: Vec3,
    to :: Vec3
  }
  deriving (Eq, Ord, Show)

mkBox :: (Int, Int, Int) -> (Int, Int, Int) -> Box3
mkBox v1 v2 = Box3 (Vec3 v1) (Vec3 v2)

null :: Box3 -> Bool
null box | box == nullBox = True
null _ = False

nullBox :: Box3
nullBox = mkBox (0, 0, 0) (0, 0, 0)

volume :: Box3 -> Int
volume (Box3 v1 v2) =
  let (Vec3 (x, y, z)) = differential v1 v2
   in abs x * abs y * abs z

intersect :: Box3 -> Box3 -> Box3
intersect (Box3 (Vec3 (x00, y00, z00)) (Vec3 (x01, y01, z01))) (Box3 (Vec3 (x10, y10, z10)) (Vec3 (x11, y11, z11))) =
  let xs = (max x00 x10, min x01 x11)
      ys = (max y00 y10, min y01 y11)
      zs = (max z00 z10, min z01 z11)
   in if all (uncurry (<)) [xs, ys, zs]
        then Box3 (Vec3 (fst xs, fst ys, fst zs)) (Vec3 (snd xs, snd ys, snd zs))
        else Box3 (Vec3 (0, 0, 0)) (Vec3 (0, 0, 0))