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

import Utils (Bit)

data Type
  = RegT Int
  | BitsT Int
  | IntT Int
  | InstT
  deriving (Eq, Ord, Show)

data EncType = EncType Type Int
  deriving Show

data BitsExpr
  = ConstBitsExpr [Bit]
  | EncBitsExpr Int String
  | ConcatBitsExpr BitsExpr BitsExpr
  | AndBitsExpr BitsExpr BitsExpr
  | OrBitsExpr BitsExpr BitsExpr
  | XorBitsExpr BitsExpr BitsExpr
  deriving Show

sizeOfEnc :: BitsExpr -> Int
sizeOfEnc (ConstBitsExpr bs)     = length bs
sizeOfEnc (EncBitsExpr n _)      = n
sizeOfEnc (ConcatBitsExpr e1 e2) = sizeOfEnc e1 + sizeOfEnc e2
sizeOfEnc (AndBitsExpr    e1 _)  = sizeOfEnc e1
sizeOfEnc (OrBitsExpr     e1 _)  = sizeOfEnc e1
sizeOfEnc (XorBitsExpr    e1 _)  = sizeOfEnc e1

findVarInEnc :: String -> Int -> BitsExpr -> Maybe (Int, Int)
findVarInEnc _   _   (ConstBitsExpr _) = Nothing
findVarInEnc var off (EncBitsExpr n var')
  | var == var' = Just (off, off + n - 1)
  | otherwise   = Nothing
findVarInEnc var off (ConcatBitsExpr e1 e2) =
  case findVarInEnc var off e1 of
    Just res -> Just res
    Nothing  -> findVarInEnc var (off + sizeOfEnc e1) e2
findVarInEnc _   _   (AndBitsExpr _ _) = Nothing
findVarInEnc _   _   (OrBitsExpr  _ _) = Nothing
findVarInEnc _   _   (XorBitsExpr _ _) = Nothing

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

data Memory = Memory String Int Int -- name, data width, address width
  deriving Show

data Proc = Proc
  { regs     :: [Reg]
  , insts    :: [Inst]
  , buttons  :: [Button]
  , memorys  :: [Memory]
  , always   :: [ImplRule]
  , encTypes :: [EncType]
  }
  deriving Show
