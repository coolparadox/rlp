-- Recursive Lenght Prefix
-- Ethereum Yellow Paper

module RlpDecode (rlpDecode) where

import Data.Word (Word8)

rlpDecode :: [Word8] -> (Either [Word8] [[Word8]], [Word8])
rlpDecode encoded@(x:_)
    | (x < 192) = (Left decodedB, remainderB)
    | otherwise = (Right decodedL, remainderL)
    where
        (decodedB, remainderB) = rlpDecodeB encoded
        (decodedL, remainderL) = rlpDecodeL encoded

rlpDecodeL :: [Word8] -> ([[Word8]], [Word8])
rlpDecodeL encoded = (decoded, remainder) where
    (ser_length, s1) = _rlpDecodeLPrefix encoded
    (serialization, remainder) = splitAt ser_length s1
    decoded = _rlpDecodeLSerialization serialization

_rlpDecodeLPrefix :: [Word8] -> (Int, [Word8])
_rlpDecodeLPrefix s@(x:xs)
    | (x < 192) = error "invalid RLP item prefix"
    | (x < 248) = (fromIntegral x - 192, xs)
    | otherwise = (len, remainder)
    where
        lenght_of_length = fromIntegral x - 247
        (be_encoding_of_length, remainder) = splitAt lenght_of_length xs
        len = beDecode be_encoding_of_length

_rlpDecodeLSerialization :: [Word8] -> [[Word8]]
_rlpDecodeLSerialization [] = []
_rlpDecodeLSerialization bytes = answer where
    (item, remainder) = rlpDecodeB bytes
    answer = item:(_rlpDecodeLSerialization remainder)

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

