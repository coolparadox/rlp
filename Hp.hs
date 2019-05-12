-- Hex-Prefix Encoding
-- Ethereum Yellow Paper

module Hp (encode, decode) where

import Byte
import Nibble
import qualified NibbleUtil as NIB

encode :: [Nibble] -> Bool -> [Byte]
encode [] t = [16 * (_flag t)]
encode nibbles@(x:xs) t
    | (length nibbles) `mod` 2 == 0 = (16 * (_flag t)):(NIB.pack nibbles)
    | otherwise = (16 * ((_flag t) + 1) + (NIB.toByte x)):(NIB.pack xs)

_flag :: Bool -> Byte
_flag True = 2
_flag False = 0

decode :: [Byte] -> ([Nibble], Bool)
decode [] = error "cannot HP decode an empty sequence"
decode (x:xs) = (nibbles, flag) where
    (flag, odd, prefix) = _unflag x
    nibbles = if odd then (prefix:ns) else ns
    ns = NIB.unpack xs

_unflag :: Byte -> (Bool, Bool, Nibble)
_unflag byte = (flag, odd, nibble) where
    flag = byte `div` 32 /= 0
    odd = (byte `div` 16) `mod` 2 /= 0
    nibble = NIB.fromByte $ byte `mod` 16

