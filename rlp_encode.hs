module RlpEncode where

import Data.Word (Word8)

rlpEncode :: Either [Word8] [[Word8]] -> [Word8]
rlpEncode (Left bytes) = rlpEncodeB bytes
rlpEncode (Right items) = rlpEncodeL items

rlpEncodeL :: [[Word8]] -> [Word8]
rlpEncodeL items = prefix ++ serialization where
    prefix = _rlpEncodeLPrefix (length serialization)
    serialization = concatMap rlpEncodeB items

_rlpEncodeLPrefix :: Int -> [Word8]
_rlpEncodeLPrefix len
    | (len < 56) = [192 + fromIntegral len]
    | (toInteger len < 2^64) = [fromIntegral (247 + length (beEncode len))] ++ beEncode len
    | otherwise = error "struct sequence too large"

rlpEncodeB :: [Word8] -> [Word8]
rlpEncodeB bytes
    | (length bytes == 0) = error "empty byte array"
    | (length bytes == 1 && head bytes < 128) = bytes
    | (length bytes < 56) = fromIntegral (128 + length bytes):bytes
    | (toInteger (length bytes) < 2^64) = [fromIntegral (183 + length (beEncode (length bytes)))] ++ beEncode (length bytes) ++ bytes
    | otherwise = error "byte sequence too large"

beEncode :: Int -> [Word8]
beEncode n
    | (n < 0) = error "out of range"
    | otherwise = reverse (_leEncode n)

_leEncode :: Int -> [Word8]
_leEncode n
    | (n < 256) = [fromIntegral n]
    | otherwise = fromIntegral (n `mod` 256):(_leEncode (fromIntegral (n `div` 256)))

