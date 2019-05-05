-- Representation of a nibble

module Nibble where

newtype Nibble = Nibble Int deriving (Bounded, Eq, Ord)

toNibble :: Int -> Nibble
toNibble x
    | x < 0 || x > 15 = error "Nibble out of range"
    | otherwise = Nibble x

fromNibble :: Nibble -> Int
fromNibble (Nibble x) = x

instance Show Nibble where
    show x = show (fromNibble x)

instance Num Nibble where
    fromInteger x = toNibble (fromInteger x)
    x + y = toNibble (fromNibble x + fromNibble y)
    x - y = toNibble (fromNibble x + fromNibble y)
    x * y = toNibble (fromNibble x * fromNibble y)
    abs x = x
    signum x
        | x == 0 = 0
        | otherwise = 1

