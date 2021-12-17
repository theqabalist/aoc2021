module Days.Day17 where

import Days.Day17.Probe (fireProbe, intersectsTarget, intersectsTargetY, maxHeight)
import Days.Day17.Target
import Prelude

partOne :: Target -> Int
partOne t@Target {ymin} =
  let candidates = filter (intersectsTargetY t) $ fireProbe 0 <$> [0 .. (- ymin)]
      maxHeights = maxHeight <$> candidates
   in foldr max 0 maxHeights

partTwo :: Target -> Int
partTwo t@Target {xmax, ymin} =
  let probes = fireProbe <$> [0 .. xmax] <*> [ymin .. (- ymin)]
   in length $ filter (intersectsTarget t) probes