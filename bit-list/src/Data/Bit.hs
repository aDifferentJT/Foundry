{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings #-}

{-|
Module      : Data.Bit
Description : A single bit
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Data.Bit
  ( Bit(Zero, One)
  ) where

import ClassyPrelude

import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

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

