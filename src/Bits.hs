{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings #-}

{-|
Module      : Bits
Description : Represent numbers as bits
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Bits
  ( Bit(Zero, One)
  , Endianness(Little, Big)
  , bitsToInt
  , intToBits
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List (foldl, unfoldr)

import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

import ClassyPrelude

-- | A single bit
data Bit
  = Zero -- ^ A 0 bit
  | One  -- ^ A 1 bit
  deriving (Eq, Ord, Enum)

instance Show Bit where
  show Zero = "0"
  show One  = "1"

instance Read Bit where
  readPrec = ReadPrec.get >>= \case
    '0' -> return Zero
    '1' -> return One
    _   -> ReadPrec.pfail

-- | The different endiannesses
data Endianness
  = Little
  | Big

-- | Turn some bits into a number
bitsToInt :: Endianness -> [Bit] -> Int
bitsToInt e = case e of
  Little -> foldl (flip f) 0
  Big    -> foldr f 0
  where f :: Bit -> Int -> Int
        f b = (fromEnum b .|.) . flip shiftL 1

-- | Turn a number into bits
intToBits :: Endianness -> Int -> [Bit]
intToBits Little = unfoldr f
  where f :: Int -> Maybe (Bit, Int)
        f 0 = Nothing
        f x = Just (toEnum (x .&. 1), shiftR x 1)
intToBits Big    = reverse . intToBits Little

