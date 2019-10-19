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

import Utils (Bit)

import Control.Lens (makeLenses)

-- | A declaration of the width of a register
data RegType = RegType Text Int
  deriving Show

-- | A declaration of the arguments of an instruction
data InstType = InstType Text [Type]
  deriving Show

-- | A declaration of the physical index of a button
data ButtonType = ButtonType Text Int
  deriving Show

-- | A definition of an encoding of a register or an instruction
data Enc
  = RegEnc Text [Bit]
  | InstEnc Text [Text] ([Bit], BitsExpr)
  deriving Show

-- | A definition of an implementation of an instruction or a button
data Impl
  = InstImpl Text [Text] [ImplRule]
  | ButtonImpl Text [ImplRule]
  deriving Show

-- | A data structure built by a single pass through the file
data RawProc = RawProc
  { _rawRegs     :: Maybe [RegType]    -- ^ The register block
  , _rawInsts    :: Maybe [InstType]   -- ^ The instruction block
  , _rawButtons  :: Maybe [ButtonType] -- ^ The button blocks
  , _rawMemorys  :: Maybe [Memory]     -- ^ The memory blocks
  , _rawEncTypes :: [EncType]          -- ^ The encoding types
  , _rawEncs     :: [Enc]              -- ^ The encodings
  , _rawImpls    :: [Impl]             -- ^ The implementations
  , _rawLedImpls :: Maybe [LedImpl]    -- ^ The led blocks
  }
  deriving Show

-- | The initial `RawProc` to be added to as we parse the file
initialProc :: RawProc
initialProc = RawProc Nothing Nothing Nothing Nothing [] [] [] Nothing

makeLenses ''RawProc
