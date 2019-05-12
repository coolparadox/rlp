-- Merkle Patricia trie
-- Ethereum Yellow Paper

module MerklePatricia where

import Byte
import Nibble
import qualified NibbleUtil as NIB
import Crypto.Hash.Keccak (keccak256)
import qualified Data.ByteString as BS
import qualified Rlp as RLP
import qualified Hp as HP

type Key = [Byte]
type Value = [Byte]
type Input = [(Key, Value)]

type Key4 = [Nibble]
type Input4 = [(Key4, Value)]

toInput4 :: Input -> Input4
toInput4 = map toKey4Pair

toKey4Pair :: (Key, Value) -> (Key4, Value)
toKey4Pair (k, v) = (NIB.unpack k, v)

trie :: Input -> BS.ByteString
trie input = keccak256 $ c input 0

c :: Input -> Int -> BS.ByteString
c [] _ = error "empty input"
c [(k, v)] i
    | i >= 0 && i < (length nibbles) = BS.pack $ RLP.encode $ Right [hp, v]
    | otherwise = error "trie depth out of range"
    where
        hp = HP.encode piece True
        (_, piece) = splitAt i nibbles
        nibbles = NIB.unpack k
