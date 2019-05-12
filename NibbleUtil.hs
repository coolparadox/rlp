-- Nibble utilities

module NibbleUtil where

import Byte
import Nibble

fromByte :: Byte -> Nibble
fromByte x
    | x < 16 = fromInteger (toInteger x)
    | otherwise = error "Nibble overflow"

toByte :: Nibble -> Byte
toByte x = fromInteger (toInteger (fromNibble x))

pack :: [Nibble] -> [Byte]
pack [] = []
pack [_] = error "non even Nibble list"
pack (x1:x2:others) = (16 * (toByte x1) + (toByte x2)):(pack others)

unpack :: [Byte] -> [Nibble]
unpack [] = []
unpack (x:xs) = (fromByte (x `div` 16)):(fromByte (x `mod` 16)):(unpack xs)

