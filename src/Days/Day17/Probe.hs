module Days.Day17.Probe where

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Days.Day17.Target
import Prelude

data Tracking = Tracking
  { vx :: Int,
    vy :: Int,
    x0 :: Int,
    y0 :: Int
  }
  deriving (Show)

newtype Probe = Probe [Tracking]
  deriving (Show)

step :: Tracking -> Tracking
step (Tracking vx vy x0 y0) =
  let x1 = x0 + vx
      y1 = y0 + vy
      vx1
        | vx > 0 = vx - 1
        | vx < 0 = vx + 1
        | otherwise = 0
      vy1 = vy - 1
   in Tracking vx1 vy1 x1 y1

fireProbe :: Int -> Int -> Probe
fireProbe vx vy = Probe $ iterate step (Tracking vx vy 0 0)

maxHeight :: Probe -> Int
maxHeight (Probe trace) =
  let differentials = zipWith (\Tracking {y0 = y00} Tracking {y0 = y1} -> (y00, y1)) trace (drop 1 trace)
      (y0, _) = fromJust $ find (\(y00, y1) -> y1 < y00) differentials
   in y0

initialConditions :: Probe -> Tracking
initialConditions (Probe []) = undefined
initialConditions (Probe (t : _)) = t

intersectsTarget :: Target -> Probe -> Bool
intersectsTarget target (Probe trace) =
  let finiteTrace = takeWhile (\Tracking {x0, y0} -> not $ overshot (x0, y0) target) trace
   in any (\Tracking {x0, y0} -> within (x0, y0) target) finiteTrace

intersectsTargetY :: Target -> Probe -> Bool
intersectsTargetY target (Probe trace) =
  let finiteTrace = takeWhile (\Tracking {y0} -> not $ overshotY y0 target) trace
   in any (\Tracking {y0} -> withinY y0 target) finiteTrace