{-# LANGUAGE TemplateHaskell #-}

module Parser.AST
  ( RegType(..)
  , InstType(..)
  , ButtonType(..)
  , UnsizedBitsExpr(..)
  , Enc(..)
  , Impl(..)
  , RawProc(..)
  , rawRegs
  , rawInsts
  , rawButtons
  , rawMemorys
  , rawInstRule
  , rawEncTypes
  , rawEncs
  , rawImpls
  ) where

import Parser.AlexPosn (Locatable)

import Proc
  ( Type
  , EncType
  , BitsExpr
  , ImplRule
  , InstRule
  , Memory
  )

import Utils (Bit)

import Control.Lens (makeLenses)

data RegType = RegType String Int
  deriving Show

data InstType = InstType String [Type]
  deriving Show

data ButtonType = ButtonType String Int
  deriving Show

data UnsizedBitsExpr
  = UnsizedConstBitsExpr (Locatable [Bit])
  | UnsizedEncBitsExpr (Locatable String)
  | UnsizedConcatBitsExpr (Locatable UnsizedBitsExpr) (Locatable UnsizedBitsExpr)
  | UnsizedAndBitsExpr (Locatable UnsizedBitsExpr) (Locatable UnsizedBitsExpr)
  | UnsizedOrBitsExpr (Locatable UnsizedBitsExpr) (Locatable UnsizedBitsExpr)
  | UnsizedXorBitsExpr (Locatable UnsizedBitsExpr) (Locatable UnsizedBitsExpr)
  deriving Show

data Enc
  = RegEnc String [Bit]
  | InstEnc String [String] ([Bit], BitsExpr)
  deriving Show

data Impl
  = InstImpl String [String] [ImplRule]
  | ButtonImpl String [ImplRule]
  deriving Show

data RawProc = RawProc
  { _rawRegs     :: [[RegType]]
  , _rawInsts    :: [[InstType]]
  , _rawButtons  :: [[ButtonType]]
  , _rawMemorys  :: [[Memory]]
  , _rawInstRule :: [InstRule]
  , _rawEncTypes :: [EncType]
  , _rawEncs     :: [Enc]
  , _rawImpls    :: [Impl]
  }
  deriving Show

makeLenses ''RawProc
