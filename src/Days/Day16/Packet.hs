module Days.Day16.Packet where

import Control.Applicative (many)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Binary (Get, Word16, Word8, getWord8)
import Data.Binary.Get (runGet)
import Data.Bits (Bits (shift, (.&.), (.|.)))
import Data.ByteString.Lazy (ByteString, pack)
import Data.Foldable (foldl')
import Data.List.Split (chunksOf)
import Prelude

data Packet = Literal Int Integer | Op Int Int [Packet]
  deriving (Show)

totalAllVersions :: Packet -> Int
totalAllVersions (Literal version _) = version
totalAllVersions (Op version _ ps) = version + sum (totalAllVersions <$> ps)

interpretPacket :: Packet -> Integer
interpretPacket (Literal _ v) = v
interpretPacket (Op _ _ []) = error "Something went wrong parsing your packet structure"
interpretPacket (Op _ 0 [p]) = interpretPacket p
interpretPacket (Op _ 0 ps) = sum $ interpretPacket <$> ps
interpretPacket (Op _ 1 [p]) = interpretPacket p
interpretPacket (Op _ 1 ps) = product $ interpretPacket <$> ps
interpretPacket (Op _ 2 [p]) = interpretPacket p
interpretPacket (Op _ 2 (p : ps)) = foldr min (interpretPacket p) (interpretPacket <$> ps)
interpretPacket (Op _ 3 [p]) = interpretPacket p
interpretPacket (Op _ 3 (p : ps)) = foldr max (interpretPacket p) (interpretPacket <$> ps)
interpretPacket (Op _ 5 [_]) = error "Something went wrong parsing your packet structure"
interpretPacket (Op _ 5 (p1 : p2 : _)) =
  let ip1 = interpretPacket p1
      ip2 = interpretPacket p2
   in if ip1 > ip2 then 1 else 0
interpretPacket (Op _ 6 [_]) = error "Something went wrong parsing your packet structure"
interpretPacket (Op _ 6 (p1 : p2 : _)) =
  let ip1 = interpretPacket p1
      ip2 = interpretPacket p2
   in if ip1 < ip2 then 1 else 0
interpretPacket (Op _ 7 [_]) = error "Something went wrong parsing your packet structure"
interpretPacket (Op _ 7 (p1 : p2 : _)) =
  let ip1 = interpretPacket p1
      ip2 = interpretPacket p2
   in if ip1 == ip2 then 1 else 0
interpretPacket p = error ("Cannot execute packet '" <> show p <> "'")

type CrazyParser = StateT (Int, Maybe Word8) Get

newtype Bit = Bit Word8
  deriving (Show)

bitRange :: Int -> Int -> Word8 -> [Bit]
bitRange from to word =
  let rightOffset = (8 - to) `mod` 8
      range = to - from
      mask = shift (2 ^ range - 1) rightOffset
      limitedWord = shift (word .&. mask) (- rightOffset) -- foldl' (\acc i -> (0x1 .&. shift word (- i)) : acc) []
   in Bit . (1 .&.) . shift limitedWord . (* (-1)) <$> reverse [0 .. (range - 1)]

unpackWord :: Word8 -> [Bit]
unpackWord = bitRange 0 8

packWord8 :: [Bit] -> Word8
packWord8 bits | length bits > 8 = error "too many bits for word8"
packWord8 bits = foldl' (\word (Bit bit) -> shift word 1 .|. bit) 0 bits

packWord16 :: [Bit] -> Word16
packWord16 bits | length bits > 16 = error "too many bits for word16"
packWord16 bits = foldl' (\word (Bit bit) -> shift word 1 .|. fromIntegral bit) 0 bits

packUnbounded :: [Bit] -> Integer
packUnbounded = foldl' (\v (Bit bit) -> shift v 1 .|. fromIntegral bit) 0

getNBits :: Int -> CrazyParser [Bit]
getNBits n = do
  (pointer, prev) <- get
  curr <- maybe (lift getWord8) pure prev
  let bitsRemainingInByte = 8 - pointer
  let bitsToRead = if n - bitsRemainingInByte < 0 then 0 else n - bitsRemainingInByte
  let bytesToRead = ceiling ((fromIntegral bitsToRead :: Double) / 8) :: Int
  bytes <- lift $ traverse (const getWord8) [0 .. (bytesToRead - 1)]
  let moved = (pointer + n) `mod` 8
  let next
        | n < bitsRemainingInByte = Just curr
        | moved == 0 = Nothing
        | otherwise = Just $ last bytes
  let fullBytes = if n <= bitsRemainingInByte then [] else maybe bytes (const $ init bytes) next
  let frontBits = bitRange pointer (pointer + if n <= bitsRemainingInByte then n else bitsRemainingInByte) curr
  let fullBits = fullBytes >>= unpackWord
  let tailBits = if n <= bitsRemainingInByte then [] else maybe [] (bitRange 0 moved) next
  put (moved, next)
  pure $ frontBits ++ fullBits ++ tailBits

packByteString :: [Bit] -> ByteString
packByteString bits =
  let padded = bits ++ (Bit <$> replicate (8 - length bits `mod` 8) 0)
   in pack $ packWord8 <$> chunksOf 8 padded

getLiteral :: (Integer -> Packet) -> CrazyParser Packet
getLiteral construct = construct . packUnbounded <$> getLiteral' []
  where
    getLiteral' collected = do
      continue <- (== 1) . packWord8 <$> getNBits 1
      chunk <- (collected ++) <$> getNBits 4
      if continue then getLiteral' chunk else pure chunk

getOp1 :: ([Packet] -> Packet) -> CrazyParser Packet
getOp1 construct =
  construct <$> do
    window <- fromIntegral . packWord16 <$> getNBits 15
    fromWindow <- packByteString <$> getNBits window
    let internal = runGet (evalStateT (many getPacket) (0, Nothing)) fromWindow
    pure internal

getOp2 :: ([Packet] -> Packet) -> CrazyParser Packet
getOp2 construct = do
  (howMany :: Integer) <- fromIntegral . packWord16 <$> getNBits 11
  construct <$> traverse (const getPacket) [0 .. (howMany - 1)]

getOp :: Int -> Int -> CrazyParser Packet
getOp version packetType = do
  isOp1 <- (== 0) . packWord8 <$> getNBits 1
  if isOp1
    then getOp1 (Op version packetType)
    else getOp2 (Op version packetType)

getPacket :: CrazyParser Packet
getPacket = do
  version <- fromIntegral . packWord8 <$> getNBits 3
  packetType <- fromIntegral . packWord8 <$> getNBits 3
  if packetType == 4
    then getLiteral (Literal version)
    else getOp version packetType