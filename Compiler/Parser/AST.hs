module Parser.AST
  ( RegType(..)
  , InstType(..)
  , ButtonType(..)
  , UnsizedBitsExpr(..)
  , Enc(..)
  , Impl(..)
  , RawProc(..)
  , UnsizedInst(..)
  , UnsizedProc(..)
  , Type(..)
  , EncType(..)
  , BitsExpr(..)
  , Op(..)
  , Expr(..)
  , BoolExpr(..)
  , LValue(..)
  , ImplRule(..)
  , Reg(..)
  , Inst(..)
  , Button(..)
  , Memory(..)
  , Proc(..)
  ) where

import Proc

import Utils (Bit(..))

data RegType = RegType String Int
  deriving Show

data InstType = InstType String [Type]
  deriving Show

data ButtonType = ButtonType String Int
  deriving Show

data UnsizedBitsExpr
  = UnsizedConstBitsExpr [Bit]
  | UnsizedEncBitsExpr String
  | UnsizedConcatBitsExpr UnsizedBitsExpr UnsizedBitsExpr
  | UnsizedAndBitsExpr UnsizedBitsExpr UnsizedBitsExpr
  | UnsizedOrBitsExpr UnsizedBitsExpr UnsizedBitsExpr
  | UnsizedXorBitsExpr UnsizedBitsExpr UnsizedBitsExpr
  deriving Show

data Enc
  = RegEnc String [Bit]
  | InstEnc String [String] ([Bit], UnsizedBitsExpr)
  deriving Show

data Impl
  = InstImpl String [String] [ImplRule]
  | ButtonImpl String [ImplRule]
  deriving Show

data RawProc = RawProc 
  { rawRegs     :: [[RegType]]
  , rawInsts    :: [[InstType]]
  , rawButtons  :: [[ButtonType]]
  , rawMemory   :: [[Memory]]
  , rawEncTypes :: [EncType]
  , rawEncs     :: [Enc]
  , rawImpls    :: [Impl]
  }
  deriving Show

data UnsizedInst = UnsizedInst String [Type] ([String], [ImplRule]) ([String], ([Bit], UnsizedBitsExpr))
  deriving Show

data UnsizedProc = UnsizedProc [Reg] [UnsizedInst] [Button] [Memory] [EncType]
  deriving Show

