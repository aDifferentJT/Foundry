{-|
Module      : Verilog.AST
Description : An AST for Verilog
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Verilog.AST
  ( Verilog(..)
  , Expr(..)
  ) where

import Utils (Bit)

data Verilog
  = RawVerilog String
  | Comment String
  | Seq [Verilog]
  | Include String
  | Define String [String] String
  | Module String [String] Verilog
  | Wire Int String (Maybe Int) (Maybe Expr)
  | Reg Int String (Maybe Int) (Maybe Expr)
  | Always String [(Maybe Expr, Expr, Expr)]
  | Assign Expr Expr

data Expr
  = RawExpr String
  | Literal Int
  | Variable String
  | Bits [Bit]
  | UnaryOp String Expr
  | BinaryOp Expr String Expr
  | TernaryOp Expr Expr Expr
  | MultiCond Expr [(Expr, Expr)]
  | FoldR String Expr [Expr]
  | UndefinedBehaviour
  deriving (Eq, Ord)

