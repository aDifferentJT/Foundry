{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import ClassyPrelude

import Data.Word.Encode

import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ word8Properties
  , word16Properties
  , word32Properties
  , word64Properties
  ]

instance Show Endianness where
  show Little = "Little"
  show Big    = "Big"

instance Arbitrary Endianness where
  arbitrary = elements [Little, Big]

word8Properties :: TestTree
word8Properties = testGroup "BitList"
  [ testProperty "decodeWord8 e . encodeWord8 e == id" $ \e n ->
    (decodeWord8 e . encodeWord8 e . getNonNegative $ n) == getNonNegative n
  , testProperty "encodeWord8 e . decodeWord8 e == id" $ \e -> forAll (map pack . vector $ 1) $ \bs ->
    (encodeWord8 e . decodeWord8 e $ bs) == bs
  ]

word16Properties :: TestTree
word16Properties = testGroup "BitList"
  [ testProperty "decodeWord16 e . encodeWord16 e == id" $ \e n ->
    (decodeWord16 e . encodeWord16 e . getNonNegative $ n) == getNonNegative n
  , testProperty "encodeWord16 e . decodeWord16 e == id" $ \e -> forAll (map pack . vector $ 2) $ \bs ->
    (encodeWord16 e . decodeWord16 e $ bs) == bs
  ]

word32Properties :: TestTree
word32Properties = testGroup "BitList"
  [ testProperty "decodeWord32 e . encodeWord32 e == id" $ \e n ->
    (decodeWord32 e . encodeWord32 e . getNonNegative $ n) == getNonNegative n
  , testProperty "encodeWord32 e . decodeWord32 e == id" $ \e -> forAll (map pack . vector $ 4) $ \bs ->
    (encodeWord32 e . decodeWord32 e $ bs) == bs
  ]

word64Properties :: TestTree
word64Properties = testGroup "BitList"
  [ testProperty "decodeWord64 e . encodeWord64 e == id" $ \e n ->
    (decodeWord64 e . encodeWord64 e . getNonNegative $ n) == getNonNegative n
  , testProperty "encodeWord64 e . decodeWord64 e == id" $ \e -> forAll (map pack . vector $ 8) $ \bs ->
    (encodeWord64 e . decodeWord64 e $ bs) == bs
  ]

