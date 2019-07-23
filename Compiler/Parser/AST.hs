module Parser.AST
  ( RegType(..)
  , InstType(..)
  , RegEnc(..)
  , UnsizedBitsExpr(..)
  , InstEnc(..)
  , InstImpl(..)
  , RawProc(..)
  , UnsizedInst(..)
  , UnsizedProc(..)
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

data UnsizedBitsExpr
  = UnsizedConstBitsExpr [Bit]
  | UnsizedEncBitsExpr String
  | UnsizedConcatBitsExpr UnsizedBitsExpr UnsizedBitsExpr
  deriving Show

data InstEnc = InstEnc String [String] ([Bit], UnsizedBitsExpr)
  deriving Show

data InstImpl = InstImpl String [String] [InstImplRule]
  deriving Show

data RawProc = RawProc [[RegType]] [[InstType]] [EncType] [RegEnc] [InstEnc] [InstImpl]
  deriving Show

data UnsizedInst = UnsizedInst String [Type] ([String], [InstImplRule]) ([String], ([Bit], UnsizedBitsExpr))
  deriving Show

data UnsizedProc = UnsizedProc [Reg] [UnsizedInst] [EncType]
  deriving Show

