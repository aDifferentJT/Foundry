module Parser.AST
  ( RegType(..)
  , InstType(..)
  , ButtonType(..)
  , UnsizedBitsExpr(..)
  , Enc(..)
  , Impl(..)
  , RawProc(..)
  ) where

import Proc
  ( Type
  , EncType
  , BitsExpr
  , ImplRule
  , Memory
  )

import Utils (Bit)

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
  | InstEnc String [String] ([Bit], BitsExpr)
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

