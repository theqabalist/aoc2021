module Days.Day18.SFPair where

import Control.Applicative
import Data.Attoparsec.Text (Parser, decimal, parseOnly, string)
import Data.Either (fromRight)
import Lib (Parseable (parse))
import Prelude

sfPairParser :: Parser SFPair
sfPairParser = V <$> decimal <|> T <$> (string "[" *> sfPairParser <* string ",") <*> (sfPairParser <* string "]")

data SFPair = T SFPair SFPair | V Int
  deriving (Eq, Ord)

instance Show SFPair where
  show (V n) = show n
  show (T x y) = "(" <> show x <> "," <> show y <> ")"

instance Parseable SFPair where
  parse = fromRight (error "invalid input") . parseOnly sfPairParser

addLeft :: Int -> SFPair -> SFPair
addLeft v (T a b) = T (addLeft v a) b
addLeft v (V u) = V $ u + v

addRight :: Int -> SFPair -> SFPair
addRight v (T a b) = T a (addRight v b)
addRight v (V u) = V $ u + v

split :: SFPair -> SFPair
split (V n)
  | n >= 10 =
    let half = fromIntegral n / 2 :: Double
     in T (V (floor half)) (V (ceiling half))
split (T x y) =
  let sx = split x
      sy = split y
   in if sx == x then T x sy else T sx y
split t = t

yc :: Eq a => (a -> a) -> a -> a
yc f a = yc' a (f a)
  where
    yc' b b' | b == b' = b
    yc' _ b' = yc' b' (f b')

add :: SFPair -> SFPair -> SFPair
add x y = reduce (T x y)

(.+.) :: SFPair -> SFPair -> SFPair
(.+.) = add

infix 5 .+.

reduce :: SFPair -> SFPair
reduce = yc (split . yc explode)

explode :: SFPair -> SFPair
explode (T (T (T (T (T (V _) (V b)) x) y) z) t) = T (T (T (T (V 0) (addLeft b x)) y) z) t
explode (T (T (T (T x (T (V a) (V b))) y) z) t) = T (T (T (T (addRight a x) (V 0)) (addLeft b y)) z) t
explode (T (T (T y (T (T (V a) (V b)) x)) z) t) = T (T (T (addRight a y) (T (V 0) (addLeft b x))) z) t
explode (T (T (T y (T x (T (V a) (V b)))) z) t) = T (T (T y (T (addRight a x) (V 0))) (addLeft b z)) t
explode (T (T z (T (T (T (V a) (V b)) x) y)) t) = T (T (addRight a z) (T (T (V 0) (addLeft b x)) y)) t
explode (T (T z (T (T x (T (V a) (V b))) y)) t) = T (T z (T (T (addRight a x) (V 0)) (addLeft b y))) t
explode (T (T z (T y (T (T (V a) (V b)) x))) t) = T (T z (T (addRight a y) (T (V 0) (addLeft b x)))) t
explode (T (T z (T y (T x (T (V a) (V b))))) t) = T (T z (T y (T (addRight a x) (V 0)))) (addLeft b t)
explode (T t (T (T (T (T (V a) (V b)) x) y) z)) = T (addRight a t) (T (T (T (V 0) (addLeft b x)) y) z)
explode (T t (T (T (T x (T (V a) (V b))) y) z)) = T t (T (T (T (addRight a x) (V 0)) (addLeft b y)) z)
explode (T t (T (T y (T (T (V a) (V b)) x)) z)) = T t (T (T (addRight a y) (T (V 0) (addLeft b x))) z)
explode (T t (T (T y (T x (T (V a) (V b)))) z)) = T t (T (T y (T (addRight a x) (V 0))) (addLeft b z))
explode (T t (T z (T (T (T (V a) (V b)) x) y))) = T t (T (addRight a z) (T (T (V 0) (addLeft b x)) y))
explode (T t (T z (T (T x (T (V a) (V b))) y))) = T t (T z (T (T (addRight a x) (V 0)) (addLeft b y)))
explode (T t (T z (T y (T (T (V a) (V b)) x)))) = T t (T z (T (addRight a y) (T (V 0) (addLeft b x))))
explode (T t (T z (T y (T x (T (V a) (V _)))))) = T t (T z (T y (T (addRight a x) (V 0))))
explode t = t

magnitude :: SFPair -> Int
magnitude (V n) = n
magnitude (T x y) = 3 * magnitude x + 2 * magnitude y