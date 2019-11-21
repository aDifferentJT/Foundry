{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.Bit.List
Description : Represent numbers as bits
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Data.Bit.List
  ( Endianness(Little, Big)
  , bitsToInt
  , intToBits
  ) where

import ClassyPrelude

import Data.Bit (Bit)

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List (foldl, unfoldr)

-- | The different endiannesses
data Endianness
  = Little
  | Big

-- | Turn some bits into a number
bitsToInt :: Endianness -> [Bit] -> Int
bitsToInt e = case e of
  Little -> foldr f 0
  Big    -> foldl (flip f) 0
  where f :: Bit -> Int -> Int
        f b = (fromEnum b .|.) . flip shiftL 1

-- | Turn a number into bits
intToBits :: Endianness -> Int -> [Bit]
intToBits Little = unfoldr f
  where f :: Int -> Maybe (Bit, Int)
        f 0 = Nothing
        f n = Just (toEnum (n .&. 1), shiftR n 1)
intToBits Big    = intToBits' []
  where intToBits' :: [Bit] -> Int -> [Bit]
        intToBits' bs 0 = bs
        intToBits' bs n = intToBits' (toEnum (n .&. 1) : bs) (shiftR n 1)

