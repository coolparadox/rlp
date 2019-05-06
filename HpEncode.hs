-- Hex-Prefix Encoding
-- Ethereum Yellow Paper

module HpEncode (hpEncode) where

import Nibble
import Data.Word (Word8)

hpEncode :: [Nibble] -> Bool -> [Word8]
hpEncode [] t = [16 * (_f t)]
hpEncode nibbles@(x:xs) t
    | (length nibbles) `mod` 2 == 0 = (16 * (_f t)):(_pack nibbles)
    | otherwise = (16 * ((_f t) + 1) + (_toWord8 x)):(_pack xs)

_pack :: [Nibble] -> [Word8]
_pack [] = []
_pack [_] = error "non even Nibble list"
_pack (x1:x2:others) = (16 * (_toWord8 x1) + (_toWord8 x2)):(_pack others)

_f :: Bool -> Word8
_f True = 2
_f False = 0

_toWord8 :: Nibble -> Word8
_toWord8 x = fromInteger (toInteger (fromNibble x))

