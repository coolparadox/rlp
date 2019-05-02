module RlpEncode where

import Data.Word (Word8)

rlpEncodeB :: [Word8] -> [Word8]
rlpEncodeB x
    | (length x == 0 || length x == 1 && head x < 128) = x
    | (length x < 56) = fromIntegral (128 + length x):x
    | (toInteger (length x) < 2^64) = [fromIntegral (183 + length (bigEndian (length x)))] ++ bigEndian (length x) ++ x
    | otherwise = error "byte sequence too large"

bigEndian :: Int -> [Word8]
bigEndian n
    | (n < 0) = error "out of range"
    | otherwise = reverse (_littleEndian n)

_littleEndian :: Int -> [Word8]
_littleEndian n
    | (n < 256) = [fromIntegral n]
    | otherwise = fromIntegral (n `mod` 256):(_littleEndian (fromIntegral (n `div` 256)))

