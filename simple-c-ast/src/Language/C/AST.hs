{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{-|
Module      : Language.C.AST
Description : A simple C AST
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.C.AST
  ( CType(..)
  , CExpr(..)
  , CStmt(..)
  )
  where

import ClassyPrelude

data CType
  = CTypeIdent Text
  | CPtr CType

instance IsString CType where
  fromString = CTypeIdent . fromString

data CExpr
  = CExprIdent Text
  | CExprInt Int
  | CParen CExpr
  | CBinOp CExpr Text CExpr
  | CMonOp Text CExpr
  | CTernOp CExpr CExpr CExpr
  | CMember CExpr Text
  | CArrow CExpr Text
  | CString Text
  | CIndex CExpr Int
  | CFuncCall Text [CExpr]

instance IsString CExpr where
  fromString = CExprIdent . fromString

instance Num CExpr where
  fromInteger = CExprInt . fromInteger
  (+)    = error "Num instance only for fromInteger"
  (*)    = error "Num instance only for fromInteger"
  abs    = error "Num instance only for fromInteger"
  signum = error "Num instance only for fromInteger"
  negate = error "Num instance only for fromInteger"

data CStmt
  = CLocalInclude Text
  | CGlobalInclude Text
  | CDefine Text CExpr
  | CUndefine Text
  | CAssign CExpr CExpr
  | CTopExpr CExpr
  | CDecl CType Text (Maybe CExpr)
  | CIf [(CExpr, CStmt)] (Maybe CStmt)
  | CSwitch CExpr CStmt
  | CCase CExpr
  | CReturn CExpr
  | CBreak
  | CContinue 
  | CFuncDef CType Text [(CType, Text)] CStmt
  | CStmts [CStmt]
  | CBlock [CStmt]
  | CComment CStmt Text
  | CBlankLine
  | CNonStmt

