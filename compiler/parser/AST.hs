module AST where

import Utils(Bit(..))

newtype Var = Var String
  deriving (Eq, Ord, Show)

data Type = NonDep String
          | Dep String Int
  deriving Show

data RegType = RegType Var Type
  deriving Show

data InstType = InstType Var [Type]
  deriving Show

data EncType = EncType Type Type
  deriving Show

data RegEnc = RegEnc Var [Bit]
  deriving Show

data BitsExpr
  = ConstBitsExpr [Bit]
  | EncBitsExpr Var
  | ConcatBitsExpr BitsExpr BitsExpr
  deriving Show

data InstEnc = InstEnc Var [Var] BitsExpr
  deriving Show

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving Show

data Expr
  = VarExpr Var
  | ConstExpr Int
  | OpExpr Op Expr Expr
  deriving Show

data InstImplRule = InstImplRule Var Expr
  deriving Show

data InstImpl = InstImpl Var [Var] [InstImplRule]
  deriving Show

data RawProc = RawProc [[RegType]] [[InstType]] [EncType] [RegEnc] [InstEnc] [InstImpl]
  deriving Show

data Reg = Reg Var Type [Bit]
  deriving Show

data Inst = Inst Var [Type] ([Var], [InstImplRule]) ([Var], BitsExpr)
  deriving Show

data Proc = Proc [Reg] [Inst] [EncType]
  deriving Show

