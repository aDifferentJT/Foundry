{-# LANGUAGE TemplateHaskell #-}

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

import Proc

import Utils (Bit)

import Control.Lens (makeLenses)

-- | A declaration of the width of a register
data RegType = RegType String Int
  deriving Show

-- | A declaration of the arguments of an instruction
data InstType = InstType String [Type]
  deriving Show

-- | A declaration of the physical index of a button
data ButtonType = ButtonType String Int
  deriving Show

-- | A definition of an encoding of a register or an instruction
data Enc
  = RegEnc String [Bit]
  | InstEnc String [String] ([Bit], BitsExpr)
  deriving Show

-- | A definition of an implementation of an instruction or a button
data Impl
  = InstImpl String [String] [ImplRule]
  | ButtonImpl String [ImplRule]
  deriving Show

-- | A data structure built by a single pass through the file
data RawProc = RawProc
  { _rawRegs     :: [[RegType]]    -- ^ The register blocks
  , _rawInsts    :: [[InstType]]   -- ^ The instruction blocks
  , _rawButtons  :: [[ButtonType]] -- ^ The button blocks
  , _rawMemorys  :: [[Memory]]     -- ^ The memory blocks
  , _rawEncTypes :: [EncType]      -- ^ The encoding types
  , _rawEncs     :: [Enc]          -- ^ The encodings
  , _rawImpls    :: [Impl]         -- ^ The implementations
  , _rawLedImpls :: [[LedImpl]]    -- ^ The led blocks
  }
  deriving Show

-- | The initial `RawProc` to be added to as we parse the file
initialProc :: RawProc
initialProc = RawProc [] [] [] [] [] [] [] []

makeLenses ''RawProc
