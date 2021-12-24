module Days.Common.Func where

import Prelude

yc :: Eq a => (a -> a) -> a -> a
yc f a = yc' a (f a)
  where
    yc' b b' | b == b' = b
    yc' _ b' = yc' b' (f b')