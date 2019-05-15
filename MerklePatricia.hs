-- Merkle Patricia trie
-- Ethereum Yellow Paper

module MerklePatricia where

import Byte
import Nibble
import qualified NibbleUtil as NIB
import Crypto.Hash.Keccak (keccak256)
import qualified Data.ByteString as BS
import qualified RecursiveLengthPrefix as RLP
import qualified HexPrefix as HP

type Key = [Byte]
type Value = [Byte]
type Input = [(Key, Value)]

type Key4 = [Nibble]
type Input4 = [(Key4, Value)]

toInput4 :: Input -> Input4
toInput4 = map toKey4Pair

toKey4Pair :: (Key, Value) -> (Key4, Value)
toKey4Pair (k, v) = (NIB.unpack k, v)

trie :: Input -> [Byte]
trie input = BS.unpack $ keccak256 $ BS.pack $ c input 0

c :: Input -> Int -> [Byte]
c [] _ = error "empty input"
c [(k, v)] i
    | i >= 0 && i < (length nibbles) = RLP.encode $ Right [hp, v]
    | otherwise = error "trie depth out of range"
    where
        hp = HP.encode piece True
        (_, piece) = splitAt i nibbles
        nibbles = NIB.unpack k
c input@((k, _):others) i
    | i /= j = RLP.encode $ Right [hp, n input j]
    | otherwise = RLP.encode $ Right $ us ++ [v]
    where
        j = length $ largestCommonPrefix $ keys4 input
        hp = HP.encode piece False
        piece = take (j - i) $ drop i nibbles
        nibbles = NIB.unpack k
        us = map u [0..15]
        v = findValueByKeyLength input i
        u j = n (filter (keyAtPosIs i j) input) (i + 1)

keyAtPosIs :: Int -> Byte -> (Key, Value) -> Bool
keyAtPosIs i j (k, _) = k !! i == j

findValueByKeyLength :: Input -> Int -> Value
findValueByKeyLength [] _ = []
findValueByKeyLength ((k, v):others) i
    | length k == i = v
    | otherwise = findValueByKeyLength others i

n :: Input -> Int -> [Byte]
n [] _ = []
n input i
    | length composition < 32 = composition
    | otherwise = BS.unpack $ keccak256 $ BS.pack $ composition
    where
        composition = c input i

keys4 :: Input -> [[Nibble]]
keys4 [] = []
keys4 ((k, _):others) = (NIB.unpack k):(keys4 others)

largestCommonPrefix :: [[Nibble]] -> [Nibble]
largestCommonPrefix [] = []
largestCommonPrefix [nibbles] = nibbles
largestCommonPrefix (nibbles:others) = _largestCommonPrefix others nibbles

_largestCommonPrefix :: [[Nibble]] -> [Nibble] -> [Nibble]
_largestCommonPrefix _ [] = []
_largestCommonPrefix [] candidate = candidate
_largestCommonPrefix (nibbles:others) candidate = _largestCommonPrefix others newCandidate where
    newCandidate = commonPrefix candidate nibbles

commonPrefix :: [Nibble] -> [Nibble] -> [Nibble]
commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (x:xs) (y:ys)
    | x == y = x:(commonPrefix xs ys)
    | otherwise = []


