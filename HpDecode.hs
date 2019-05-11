-- Hex-Prefix Decoding
-- Ethereum Yellow Paper

module HpDecode where

import Nibble
import Support (Byte, nibbleUnpack, byteToNibble)

hpDecode :: [Byte] -> ([Nibble], Bool)
hpDecode [] = error "cannot HP decode an empty sequence"
hpDecode (x:xs) = (nibbles, flag) where
    (flag, odd, prefix) = _unflag x
    nibbles = if odd then (prefix:ns) else ns
    ns = nibbleUnpack xs

_unflag :: Byte -> (Bool, Bool, Nibble)
_unflag byte = (flag, odd, nibble) where
    flag = byte `div` 32 /= 0
    odd = (byte `div` 16) `mod` 2 /= 0
    nibble = byteToNibble (byte `mod` 16)

