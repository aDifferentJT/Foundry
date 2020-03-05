{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : Language.Verilog.AST
Description : An AST for Verilog
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.Verilog.AST
  ( Verilog(..)
  , Expr(..)
  ) where

import ClassyPrelude

import Data.Bit (Bit)

data Verilog
  = RawVerilog Text
  | Comment Text
  | Seq [Verilog]
  | Include Text
  | Define Text [Text] Text
  | Module Text [Text] Verilog
  | Wire Int Text (Maybe Int) (Maybe Expr)
  | Reg Int Text (Maybe Int) (Maybe Expr)
  | Initial [(Maybe Expr, Expr, Expr)]
  | Always Text [(Maybe Expr, Expr, Expr)]
  | Assign Expr Expr

data Expr
  = RawExpr Text
  | Literal Int
  | Variable Text
  | Bits [Bit]
  | Index Expr Expr Expr
  | UnaryOp Text Expr
  | BinaryOp Expr Text Expr
  | TernaryOp Expr Expr Expr
  | MultiCond Expr [(Expr, Expr)]
  | FoldR Text Expr [Expr]
  | UndefinedBehaviour
  deriving (Eq, Ord)

instance IsString Expr where
  fromString = Variable . fromString

