module AST
  ( RegType(..)
  , InstType(..)
  , RegEnc(..)
  , InstEnc(..)
  , InstImpl(..)
  , RawProc(..)
  , Type(..)
  , EncType(..)
  , BitsExpr(..)
  , Op(..)
  , Expr(..)
  , InstImplRule(..)
  , Reg(..)
  , Inst(..)
  , Proc(..)
  ) where

import Proc

import Utils (Bit(..))

data RegType = RegType String Int
  deriving Show

data InstType = InstType String [Type]
  deriving Show

data RegEnc = RegEnc String [Bit]
  deriving Show

data InstEnc = InstEnc String [String] BitsExpr
  deriving Show

data InstImpl = InstImpl String [String] [InstImplRule]
  deriving Show

data RawProc = RawProc [[RegType]] [[InstType]] [EncType] [RegEnc] [InstEnc] [InstImpl]
  deriving Show

