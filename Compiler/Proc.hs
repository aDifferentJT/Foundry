module Proc
  ( Type(..)
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

import Utils (Bit(..))

data Type
  = RegT Int
  | BitsT Int
  | IntT Int
  | InstT
  deriving (Eq, Show)

data EncType = EncType Type Int
  deriving Show

data BitsExpr
  = ConstBitsExpr [Bit]
  | EncBitsExpr Int String
  | ConcatBitsExpr BitsExpr BitsExpr
  deriving Show

data Op
  = Add
  | Sub
  | Mul
  | Div
  | ConcatBits
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  deriving Show

data BoolExpr
  = EqualityExpr Expr Expr
  | LogicalAndExpr BoolExpr BoolExpr
  | LogicalOrExpr BoolExpr BoolExpr
  deriving Show

data Expr
  = VarExpr String
  | MemAccessExpr String Expr
  | ConstExpr Int
  | BinaryConstExpr [Bit]
  | OpExpr Op Expr Expr
  | TernaryExpr BoolExpr Expr Expr
  deriving Show

data LValue
  = VarLValue String
  | MemAccessLValue String Expr
  deriving Show

data ImplRule = ImplRule LValue Expr
  deriving Show

data Reg = Reg String Int [Bit]
  deriving Show

data Inst = Inst String [Type] ([String], [ImplRule]) ([String], ([Bit], BitsExpr))
  deriving Show

data Button = Button String Int [ImplRule]
  deriving Show

data Memory = Memory String Int Int -- name, data width, register width
  deriving Show

data Proc = Proc [Reg] [Inst] [Button] [Memory] [EncType]
  deriving Show

