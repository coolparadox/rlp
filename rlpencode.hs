module RlpEncode where

import Data.Word (Word8)

rlpEncodeB :: [Word8] -> [Word8]
rlpEncodeB x
    | (length x == 0 || length x == 1 && head x < 128) = x
    | (length x < 56) = fromIntegral (128 + length x):x
    | (toInteger (length x) < 2^64) = [fromIntegral (183 + length (beEncode (length x)))] ++ beEncode (length x) ++ x
    | otherwise = error "byte sequence too large"

beEncode :: Int -> [Word8]
beEncode n
    | (n < 0) = error "out of range"
    | otherwise = reverse (_leEncode n)

_leEncode :: Int -> [Word8]
_leEncode n
    | (n < 256) = [fromIntegral n]
    | otherwise = fromIntegral (n `mod` 256):(_leEncode (fromIntegral (n `div` 256)))

