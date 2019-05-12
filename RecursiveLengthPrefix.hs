-- Recursive Lenght Prefix
-- Ethereum Yellow Paper

module RecursiveLengthPrefix (encode, decode) where

import Byte

encode :: Either [Byte] [[Byte]] -> [Byte]
encode (Left bytes) = encodeB bytes
encode (Right items) = encodeL items

encodeL :: [[Byte]] -> [Byte]
encodeL items = prefix ++ serialization where
    prefix = encodeLPrefix (length serialization)
    serialization = concatMap encodeB items

encodeLPrefix :: Int -> [Byte]
encodeLPrefix len
    | (len < 56) = [192 + fromIntegral len]
    | (toInteger len < 2^64) = [fromIntegral (247 + length (beEncode len))] ++ beEncode len
    | otherwise = error "struct sequence too large"

encodeB :: [Byte] -> [Byte]
encodeB bytes
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

decode :: [Byte] -> (Either [Byte] [[Byte]], [Byte])
decode encoded@(x:_)
    | (x < 192) = (Left decodedB, remainderB)
    | otherwise = (Right decodedL, remainderL)
    where
        (decodedB, remainderB) = decodeB encoded
        (decodedL, remainderL) = decodeL encoded

decodeL :: [Byte] -> ([[Byte]], [Byte])
decodeL encoded = (decoded, remainder) where
    (ser_length, s1) = decodeLPrefix encoded
    (serialization, remainder) = splitAt ser_length s1
    decoded = decodeLSerialization serialization

decodeLPrefix :: [Byte] -> (Int, [Byte])
decodeLPrefix s@(x:xs)
    | (x < 192) = error "invalid RLP item prefix"
    | (x < 248) = (fromIntegral x - 192, xs)
    | otherwise = (len, remainder)
    where
        lenght_of_length = fromIntegral x - 247
        (be_encoding_of_length, remainder) = splitAt lenght_of_length xs
        len = beDecode be_encoding_of_length

decodeLSerialization :: [Byte] -> [[Byte]]
decodeLSerialization [] = []
decodeLSerialization bytes = answer where
    (item, remainder) = decodeB bytes
    answer = item:(decodeLSerialization remainder)

decodeB :: [Byte] -> ([Byte], [Byte])
decodeB [] = ([], [])
decodeB s@(x:_)
    | (x < 128) = splitAt 1 s
    | (x < 184) = decodeB128 s
    | otherwise = decodeB184 s

decodeB128 :: [Byte] -> ([Byte], [Byte])
decodeB128 s@(x:_) = (decoded, remainder) where
    (encoded, remainder) = splitAt length_of_encoded s
    length_of_encoded = 1 + length_of_decoded
    length_of_decoded = fromIntegral x - 128
    (_, decoded) = splitAt 1 encoded

decodeB184 :: [Byte] -> ([Byte], [Byte])
decodeB184 s@(x:_) = (decoded, remainder) where
    (encoded, remainder) = splitAt length_of_encoded s
    length_of_encoded = 1 + length_of_length_of_decoded + length_of_decoded
    length_of_length_of_decoded = fromIntegral x - 183
    length_of_decoded = beDecode be_encoding_of_length_of_decoded
    (be_encoding_of_length_of_decoded, _) = splitAt length_of_length_of_decoded (tail s)
    (_, decoded) = splitAt (1 + length_of_length_of_decoded) encoded

beDecode :: [Byte] -> Int
beDecode [] = error "parse error"
beDecode [x] = fromIntegral x
beDecode (x:xs) = 256 * (fromIntegral x) + (beDecode xs)

