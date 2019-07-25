module Proc
  ( Type(..)
  , EncType(..)
  , BitsExpr(..)
  , sizeOfEnc
  , findVarInEnc
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

sizeOfEnc :: BitsExpr -> Int
sizeOfEnc (ConstBitsExpr bs)     = length bs
sizeOfEnc (EncBitsExpr n _)      = n
sizeOfEnc (ConcatBitsExpr e1 e2) = sizeOfEnc e1 + sizeOfEnc e2

findVarInEnc :: String -> BitsExpr -> Maybe (Int, Int)
findVarInEnc var (ConstBitsExpr _) = Nothing
findVarInEnc var (EncBitsExpr n var')
  | var == var' = Just (0, n - 1)
  | otherwise   = Nothing
findVarInEnc var (ConcatBitsExpr e1 e2) =
  case findVarInEnc var e1 of
    Just res -> Just res
    Nothing  -> let n = sizeOfEnc e1 in (\(i, j) -> (i + n, j + n)) <$> findVarInEnc var e2

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
  | RegExpr String
  | MemAccessExpr String Expr
  | ConstExpr Int
  | BinaryConstExpr [Bit]
  | OpExpr Op Expr Expr
  | TernaryExpr BoolExpr Expr Expr
  deriving Show

data LValue
  = VarLValue String
  | RegLValue String
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

