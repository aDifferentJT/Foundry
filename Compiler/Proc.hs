module Proc
  ( Type(..)
  , EncType(..)
  , BitsExpr(..)
  , Op(..)
  , Expr(..)
  , InstImplRule(..)
  , Reg(..)
  , Inst(..)
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
  | EncBitsExpr String
  | ConcatBitsExpr BitsExpr BitsExpr
  deriving Show

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving Show

data Expr
  = VarExpr String
  | ConstExpr Int
  | OpExpr Op Expr Expr
  deriving Show

data InstImplRule = InstImplRule String Expr
  deriving Show

data Reg = Reg String Int [Bit]
  deriving Show

data Inst = Inst String [Type] ([String], [InstImplRule]) ([String], BitsExpr)
  deriving Show

data Proc = Proc [Reg] [Inst] [EncType]
  deriving Show

