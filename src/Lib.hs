{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( aperture,
    Parseable (parse),
    forkInteract,
    forkInteract',
    dualTextAdapter,
    dualTextAdapter',
    unwrap,
    knotHash,
    zeroPad,
    binWord,
  )
where

import Control.Monad (foldM_, join)
import Control.Monad.ST (runST)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.List (tails)
import Data.List.Split (chunksOf)
import Data.Text (Text, concat, pack, strip, unpack)
import Data.Text.IO (interact)
import Data.Vector.Unboxed (Vector, freeze, fromList, length, thaw, toList, (!))
import Data.Vector.Unboxed.Mutable (write)
import Numeric (showHex, showIntAtBase)
import Prelude hiding (concat, interact, length)
import qualified Prelude as P

aperture' :: Int -> [a] -> [[a]]
aperture' n = map (take n) . tails

takeLengthOf :: [a] -> [b] -> [b]
takeLengthOf = zipWith (const id)

aperture :: Int -> [a] -> [[a]]
aperture n xs = takeLengthOf (drop (n - 1) xs) (aperture' n xs)

class Parseable a where
  parse :: Text -> a

instance Parseable Text where
  parse = id

forkInteract :: (Parseable a, Show b, Parseable c, Show d) => (a -> b) -> (c -> d) -> IO ()
forkInteract f1 f2 = interact $ (\input -> concat ["part 1: ", pack . show $ f1 $ parse input, "\n\npart 2: ", pack . show $ f2 $ parse input, "\n"]) . strip

forkInteract' :: (Parseable a, Show b, Parseable c, Show d) => (a -> b) -> (c -> d) -> IO ()
forkInteract' f1 f2 = interact (\input -> concat ["part 1: ", pack . show $ f1 $ parse input, "\n\npart 2: ", pack . show $ f2 $ parse input, "\n"])

dualTextAdapter :: (Parseable a, Show b, Parseable c, Show d) => (a -> b) -> (c -> d) -> Text -> Text
dualTextAdapter f1 f2 = (\input -> concat ["part 1: ", pack . show $ f1 $ parse input, "\n\npart 2: ", pack . show $ f2 $ parse input, "\n"]) . strip

dualTextAdapter' :: (Parseable a, Show b, Parseable c, Show d) => (a -> b) -> (c -> d) -> Text -> Text
dualTextAdapter' f1 f2 input = concat ["part 1: ", pack . show $ f1 $ parse input, "\n\npart 2: ", pack . show $ f2 $ parse input, "\n"]

unwrap :: Show a => Either a b -> b
unwrap (Right x) = x
unwrap (Left x) = error (show x)

pancakeFlip :: Int -> Int -> Vector Int -> Vector Int
pancakeFlip p l v = runST $ do
  let size = length v
  let flipped = zip [0 ..] $ fmap (v !) $ reverse $ fmap (`mod` size) [p .. (p + l - 1)]
  vec <- thaw v
  foldM_ (\_ (offset, value) -> write vec ((p + offset) `mod` size) value) () flipped
  freeze vec

runFlips :: Int -> Int -> [Int] -> Vector Int -> Vector Int
runFlips position skips ls v
  | null ls = v
  | otherwise =
    let len = head ls
     in runFlips ((position + skips + len) `mod` length v) (skips + 1) (tail ls) (pancakeFlip position len v)

zeroPad :: Int -> String -> String
zeroPad n s
  | P.length s < n = zeroPad n ('0' : s)
  | otherwise = s

mapBin :: Int -> Char
mapBin 0 = '0'
mapBin 1 = '1'

binWord :: Int -> Int -> String
binWord n i = zeroPad n $ showIntAtBase 2 mapBin i ""

knotHash :: String -> String
knotHash input =
  let instructions = join . replicate 64 . flip (++) [17, 31, 73, 47, 23] . fmap ord $ input
      jumbled = toList $ runFlips 0 0 instructions (fromList [0 .. 255])
      chunked = chunksOf 16 jumbled
      reduced = fmap (\(head : rest) -> foldl' xor head rest) chunked
      converted = fmap (zeroPad 2 . (`showHex` "")) reduced
   in foldl' (++) "" converted