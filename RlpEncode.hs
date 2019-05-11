-- Recursive Lenght Prefix
-- Ethereum Yellow Paper

module RlpEncode (rlpEncode) where

import Support (Byte)

rlpEncode :: Either [Byte] [[Byte]] -> [Byte]
rlpEncode (Left bytes) = rlpEncodeB bytes
rlpEncode (Right items) = rlpEncodeL items

rlpEncodeL :: [[Byte]] -> [Byte]
rlpEncodeL items = prefix ++ serialization where
    prefix = _rlpEncodeLPrefix (length serialization)
    serialization = concatMap rlpEncodeB items

_rlpEncodeLPrefix :: Int -> [Byte]
_rlpEncodeLPrefix len
    | (len < 56) = [192 + fromIntegral len]
    | (toInteger len < 2^64) = [fromIntegral (247 + length (beEncode len))] ++ beEncode len
    | otherwise = error "struct sequence too large"

rlpEncodeB :: [Byte] -> [Byte]
rlpEncodeB bytes
    | (length bytes == 0) = error "empty byte array"
    | (length bytes == 1 && head bytes < 128) = bytes
    | (length bytes < 56) = fromIntegral (128 + length bytes):bytes
    | (toInteger (length bytes) < 2^64) = [fromIntegral (183 + length (beEncode (length bytes)))] ++ beEncode (length bytes) ++ bytes
    | otherwise = error "byte sequence too large"

beEncode :: Int -> [Byte]
beEncode n
    | (n < 0) = error "out of range"
    | otherwise = reverse (_leEncode n)

_leEncode :: Int -> [Byte]
_leEncode n
    | (n < 256) = [fromIntegral n]
    | otherwise = fromIntegral (n `mod` 256):(_leEncode (fromIntegral (n `div` 256)))

