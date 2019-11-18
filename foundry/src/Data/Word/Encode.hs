{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings, RankNTypes, TupleSections, TypeFamilies, UndecidableInstances #-}

{-|
Module      : Data.Word.Encode
Description : Encode words into bytestrings
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Data.Word.Encode
  ( Endianness(..)
  , encodeWord8
  , encodeWord16
  , encodeWord32
  , encodeWord64
  , decodeWord8
  , decodeWord16
  , decodeWord32
  , decodeWord64
  ) where

import ClassyPrelude

import Data.Bit.List (Endianness(..))

import Control.Arrow ((***))
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Word

infixr 0 .$.
(.$.) :: (a -> b) -> (a, a) -> (b, b)
(.$.) f = f *** f

encodeWord8 :: Endianness -> Word8 -> ByteString
encodeWord8 _ = pack . (:[])

encodeWord16 :: Endianness -> Word16 -> ByteString
encodeWord16 e x = case e of
    Little -> ls ++ bs
    Big    -> bs ++ ls
  where ls = encodeWord8 e (fromIntegral x)
        bs = encodeWord8 e (fromIntegral . shiftR x $ 8)

encodeWord32 :: Endianness -> Word32 -> ByteString
encodeWord32 e x = case e of
    Little -> ls ++ bs
    Big    -> bs ++ ls
  where ls = encodeWord16 e (fromIntegral x)
        bs = encodeWord16 e (fromIntegral . shiftR x $ 16)

encodeWord64 :: Endianness -> Word64 -> ByteString
encodeWord64 e x = case e of
    Little -> ls ++ bs
    Big    -> bs ++ ls
  where ls = encodeWord32 e (fromIntegral x)
        bs = encodeWord32 e (fromIntegral . shiftR x $ 32)

decodeWord8 :: Endianness -> ByteString -> Word8
decodeWord8 _ = fromMaybe 0 . headMay . unpack

decodeWord16 :: Endianness -> ByteString -> Word16
decodeWord16 e bs = case e of
    Little -> x .|. shiftL y 8
    Big    -> shiftL x 8 .|. y
  where (x, y) = fromIntegral . decodeWord8 e .$. splitAt 1 bs

decodeWord32 :: Endianness -> ByteString -> Word32
decodeWord32 e bs = case e of
    Little -> x .|. shiftL y 16
    Big    -> shiftL x 16 .|. y
  where (x, y) = fromIntegral . decodeWord16 e .$. splitAt 2 bs

decodeWord64 :: Endianness -> ByteString -> Word64
decodeWord64 e bs = case e of
    Little -> x .|. shiftL y 32
    Big    -> shiftL x 32 .|. y
  where (x, y) = fromIntegral . decodeWord32 e .$. splitAt 4 bs

