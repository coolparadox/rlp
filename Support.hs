-- Support functions

module Support where

import Nibble
import Data.Word (Word8)

type Byte = Word8

byteToNibble :: Byte -> Nibble
byteToNibble x
    | x < 16 = fromInteger (toInteger x)
    | otherwise = error "Nibble overflow"

nibbleToByte :: Nibble -> Byte
nibbleToByte x = fromInteger (toInteger (fromNibble x))

nibblePack :: [Nibble] -> [Byte]
nibblePack [] = []
nibblePack [_] = error "non even Nibble list"
nibblePack (x1:x2:others) = (16 * (nibbleToByte x1) + (nibbleToByte x2)):(nibblePack others)

nibbleUnpack :: [Byte] -> [Nibble]
nibbleUnpack [] = []
nibbleUnpack (x:xs) = (byteToNibble (x `div` 16)):(byteToNibble (x `mod` 16)):(nibbleUnpack xs)

