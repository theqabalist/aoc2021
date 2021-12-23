module Days.Day22.Expression where

import Days.Day22.Box3 (Box3, null)
import qualified Days.Day22.Box3 as B3
import Prelude hiding (null)

data Expression = Base Box3 | Union Expression Expression | Difference Expression Expression | Intersection Expression Expression
  deriving (Show, Eq, Ord)

normalForm :: Expression -> Expression
normalForm (Intersection (Base b1) (Base b2)) = Base $ B3.intersect b1 b2
normalForm (Intersection (Union e1 e2) e3) = Union (Intersection e1 e3) (Intersection e2 e3)
normalForm (Intersection e3 (Union e1 e2)) = Union (Intersection e1 e3) (Intersection e2 e3)
normalForm (Intersection (Difference e1 e2) e3) = Difference (Intersection e1 e3) (Intersection e2 e3)
normalForm (Intersection e3 (Difference e1 e2)) = Difference (Intersection e1 e3) (Intersection e2 e3)
normalForm (Intersection e1 e2) = Intersection (normalForm e1) (normalForm e2)
normalForm a = a

volume :: Expression -> Int
volume (Base box) | null box = 0
volume (Base box) = B3.volume box
volume (Intersection (Base box) _) | null box = 0
volume (Intersection _ (Base box)) | null box = 0
volume e@(Intersection _ _) = volume $ normalForm e
volume (Union (Base box) e1) | null box = volume e1
volume (Union e1 (Base box)) | null box = volume e1
volume (Difference (Base box) _) | null box = 0
volume (Difference e1 (Base box)) | null box = volume e1
volume (Union e1 e2) = volume e1 + volume e2 - volume (Intersection e1 e2)
volume (Difference e1 e2) = volume e1 - volume (Intersection e1 e2)