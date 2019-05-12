-- Merkle Patricia trie
-- Ethereum Yellow Paper

module MerklePatricia where

import Nibble
import Support (Byte, nibbleUnpack)
import Crypto.Hash.Keccak (keccak256)
import qualified Data.ByteString as BS
import RlpEncode (rlpEncode)
import HpEncode (hpEncode)

type Key = [Byte]
type Value = [Byte]
type Input = [(Key, Value)]

type Key4 = [Nibble]
type Input4 = [(Key4, Value)]

toInput4 :: Input -> Input4
toInput4 = map toKey4Pair

toKey4Pair :: (Key, Value) -> (Key4, Value)
toKey4Pair (k, v) = (nibbleUnpack k, v)

trie :: Input -> BS.ByteString
trie input = keccak256 $ c input 0

c :: Input -> Int -> BS.ByteString
c [] _ = error "empty input"
c [(k, v)] i = BS.pack $ rlpEncode $ Right [hp, v] where
    hp = hpEncode piece True
    (_, piece) = splitAt i nibbles
    nibbles = nibbleUnpack k
