module RlpDecode where

import Data.Word (Word8)

rlpDecodeB :: [Word8] -> ([Word8], [Word8])
rlpDecodeB [] = ([], [])
rlpDecodeB s@(x:_)
    | (x < 128) = splitAt 1 s
    | (x < 184) = _rlpDecodeB128 s
    | otherwise = _rlpDecodeB184 s

_rlpDecodeB128 :: [Word8] -> ([Word8], [Word8])
_rlpDecodeB128 s@(x:_) = (decoded, remainder) where
    (encoded, remainder) = splitAt length_of_encoded s
    length_of_encoded = 1 + length_of_decoded
    length_of_decoded = fromIntegral x - 128
    (_, decoded) = splitAt 1 encoded

_rlpDecodeB184 :: [Word8] -> ([Word8], [Word8])
_rlpDecodeB184 s@(x:_) = (decoded, remainder) where
    (encoded, remainder) = splitAt length_of_encoded s
    length_of_encoded = 1 + length_of_length_of_decoded + length_of_decoded
    length_of_length_of_decoded = fromIntegral x - 183
    length_of_decoded = beDecode be_encoding_of_length_of_decoded
    (be_encoding_of_length_of_decoded, _) = splitAt length_of_length_of_decoded (tail s)
    (_, decoded) = splitAt (1 + length_of_length_of_decoded) encoded

beDecode :: [Word8] -> Int
beDecode [] = error "parse error"
beDecode [x] = fromIntegral x
beDecode (x:xs) = 256 * (fromIntegral x) + (beDecode xs)

