-- Hex-Prefix Encoding
-- Ethereum Yellow Paper

module HpEncode (hpEncode) where

import Nibble
import Support (Byte, nibblePack, nibbleToByte)

hpEncode :: [Nibble] -> Bool -> [Byte]
hpEncode [] t = [16 * (_flag t)]
hpEncode nibbles@(x:xs) t
    | (length nibbles) `mod` 2 == 0 = (16 * (_flag t)):(nibblePack nibbles)
    | otherwise = (16 * ((_flag t) + 1) + (nibbleToByte x)):(nibblePack xs)

_flag :: Bool -> Byte
_flag True = 2
_flag False = 0

