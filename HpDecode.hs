-- Hex-Prefix Decoding
-- Ethereum Yellow Paper

module HpDecode where

import Nibble
import Data.Word (Word8)

hpDecode :: [Word8] -> ([Nibble], Bool)
hpDecode [] = error "cannot HP decode an empty sequence"
hpDecode (x:xs) = (nibbles, flag) where
    (flag, odd, prefix) = _unflag x
    nibbles = if odd then (prefix:ns) else ns
    ns = _unpack xs

_unflag :: Word8 -> (Bool, Bool, Nibble)
_unflag byte = (flag, odd, nibble) where
    flag = byte `div` 32 /= 0
    odd = (byte `div` 16) `mod` 2 /= 0
    nibble = _toNibble (byte `mod` 16)

_unpack :: [Word8] -> [Nibble]
_unpack [] = []
_unpack (x:xs) = (_toNibble (x `div` 16)):(_toNibble (x `mod` 16)):(_unpack xs)

_toNibble :: Word8 -> Nibble
_toNibble x
    | x < 16 = fromInteger (toInteger x)
    | otherwise = error "Nibble overflow"
