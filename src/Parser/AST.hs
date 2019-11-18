{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

{-|
Module      : Parser.AST
Description : The data structure representing a processor definition file
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental

This is the data structure representing a processor definition file, it re-exports everything from the "Proc" module
-}
module Parser.AST
  ( module Proc
  , RegType(..)
  , InstType(..)
  , ButtonType(..)
  , MaybeBitsExpr(..)
  , sizeOfMaybeEnc
  , Enc(..)
  , Impl(..)
  , RawProc(
      RawProc
      , _rawRegs
      , _rawInsts
      , _rawButtons
      , _rawMemorys
      , _rawEncTypes
      , _rawEncs
      , _rawImpls
      , _rawLedImpls
    )
  , initialProc
  -- * Lenses
  , rawRegs
  , rawInsts
  , rawButtons
  , rawMemorys
  , rawEncTypes
  , rawEncs
  , rawImpls
  , rawLedImpls
  ) where

import ClassyPrelude

import Proc

import Bits (Bit)

import Control.Lens (makeLenses)
import qualified Data.Map as Map
import Data.Map (Map)

-- | A declaration of the width of a register
type RegType = (Text, Int)

-- | A declaration of the arguments of an instruction
type InstType = (Text, [Type])

-- | A declaration of the physical index of a button
type ButtonType = (Text, Int)

-- | An expression for some bits with maybe widths
data MaybeBitsExpr
  = MaybeConstBitsExpr [Bit]                                    -- ^ A constant array of bits
  | MaybeEncBitsExpr (Maybe Int) Text                           -- ^ The encoding of a variable with given width and identifier
  | MaybeConcatBitsExpr (Maybe Int) MaybeBitsExpr MaybeBitsExpr -- ^ Two expressions of bits concatenated with the width
  deriving Show

-- | Calculate the width of an expression of bits
sizeOfMaybeEnc :: MaybeBitsExpr -> Maybe Int
sizeOfMaybeEnc (MaybeConstBitsExpr  bs)    = Just . length $ bs
sizeOfMaybeEnc (MaybeEncBitsExpr    n _)   = n
sizeOfMaybeEnc (MaybeConcatBitsExpr n _ _) = n

-- | A definition of an encoding of a register or an instruction
data Enc
  = RegEnc [Bit]
  | InstEnc [Text] ([Bit], BitsExpr)
  deriving Show

-- | A definition of an implementation of an instruction or a button
data Impl
  = InstImpl [Text] [ImplRule]
  | ButtonImpl [ImplRule]
  deriving Show

-- | A data structure built by a single pass through the file
data RawProc = RawProc
  { _rawRegs     :: Map Text Int        -- ^ The register block
  , _rawInsts    :: Map Text [Type]     -- ^ The instruction block
  , _rawButtons  :: Map Text Int        -- ^ The button blocks
  , _rawMemorys  :: [Memory]            -- ^ The memory blocks
  , _rawEncTypes :: [EncType]           -- ^ The encoding types
  , _rawEncs     :: Map Text Enc        -- ^ The encodings
  , _rawImpls    :: Map Text Impl       -- ^ The implementations
  , _rawLedImpls :: Maybe [LedImpl]     -- ^ The led blocks
  }
  deriving Show

-- | The initial `RawProc` to be added to as we parse the file
initialProc :: RawProc
initialProc = RawProc Map.empty Map.empty Map.empty [] [] Map.empty Map.empty Nothing

makeLenses ''RawProc
