{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import ClassyPrelude

import Data.Bit
import Data.Bit.List

import Test.QuickCheck.Instances.Text ()
import qualified Test.SmallCheck.Series as SC
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.SmallCheck as SC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ bitProperties
  , bitListProperties
  ]

instance Show Endianness where
  show Little = "Little"
  show Big    = "Big"

instance QC.Arbitrary Endianness where
  arbitrary = QC.elements [Little, Big]

instance Monad m => SC.Serial m Bit where
  series = SC.generate . const $ [Zero, One]

instance QC.Arbitrary Bit where
  arbitrary = QC.elements [Zero, One]

newtype BitString = BitString { getBitString :: Text }
  deriving Show

instance Monad m => SC.Serial m BitString where
  series = SC.generate . const . map BitString $ ["0", "1"]

newtype BitList = BitList { getBitList :: [Bit] }
  deriving Show

instance QC.Arbitrary BitList where
  arbitrary = map BitList . QC.resize 30 $ QC.arbitrary

bitProperties :: TestTree
bitProperties = testGroup "Bit"
  [ SC.testProperty "readMay . tshow == Just"
    ( ( \b ->
        (readMay . tshow) b == Just b
      ) :: Bit -> Bool
    )
  , SC.testProperty "map tshow . readMay == Just"
    ( ( \b ->
        (map (tshow . (id :: Bit -> Bit)) . readMay . getBitString $ b) == (Just . getBitString $ b)
      ) :: BitString -> Bool
    )
  , QC.testProperty "t /= \"0\" && t /= \"1\" ==> null (readMay t)" $ \t ->
    let _ = t :: Text in
    t /= "0" && t /= "1" QC.==> null (readMay t :: Maybe Bit)
  , testGroup "Ord"
    [ SC.testProperty "Zero < One" (Zero < One) ]
  , testGroup "Enum"
    [ SC.testProperty "fromEnum Zero == 0" (fromEnum Zero == 0)
    , SC.testProperty "fromEnum One == 1" (fromEnum One == 1)
    ]
  ]

bitListProperties :: TestTree
bitListProperties = testGroup "BitList"
  [ QC.testProperty "bitsToInt e . intToBits e == id" $ \e n ->
    (bitsToInt e . intToBits e . QC.getNonNegative $ n) == QC.getNonNegative n
  , QC.testProperty "intToBits Big . bitsToInt Big == dropWhile (== Zero)" $
    (\bs ->
      (intToBits Big . bitsToInt Big $ bs) == dropWhile (== Zero) bs
    ) . getBitList
  , QC.testProperty "intToBits Little . bitsToInt Little == reverse . dropWhile (== Zero) . reverse" $
    (\bs ->
      (intToBits Little . bitsToInt Little $ bs) == (reverse . dropWhile (== Zero) . reverse $ bs)
    ) . getBitList
  ]

