{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, OverloadedStrings #-}

{-|
Module      : Language.Elm.AST
Description : A simple Elm AST
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.Elm.AST
  ( ElmTypeWithIdent(..)
  , ElmType
  , ElmPattern(..)
  , ElmExpr(..)
  , ElmStmt(..)
  ) where

import ClassyPrelude

data ElmTypeWithIdent a
  = ElmTypeIdent a
  | ElmFuncType (ElmTypeWithIdent a) (ElmTypeWithIdent a)
  | ElmTupleType [ElmTypeWithIdent a]
  | ElmRecordType [(a, ElmTypeWithIdent a)]
  | ElmTypeFuncAppl a [ElmTypeWithIdent a]
  deriving (Eq, Ord)

type ElmType = ElmTypeWithIdent Text

instance IsString ElmType where
  fromString = ElmTypeIdent . fromString

data ElmPattern
  = ElmPatIdent Text
  | ElmPatInt Int
  | ElmPatFuncAppl Text [ElmPattern]
  | ElmTuplePat [ElmPattern]
  | ElmCons ElmPattern ElmPattern
  | ElmListPat [ElmPattern]
  | ElmStringPat Text
  deriving (Eq, Ord)

instance IsString ElmPattern where
  fromString = ElmPatIdent . fromString

instance Num ElmPattern where
  fromInteger = ElmPatInt . fromInteger
  (+)    = error "Num instance only for fromInteger"
  (*)    = error "Num instance only for fromInteger"
  abs    = error "Num instance only for fromInteger"
  signum = error "Num instance only for fromInteger"
  negate = error "Num instance only for fromInteger"

data ElmExpr
  = ElmExprIdent Text
  | ElmExprInt Int
  | ElmBinOp ElmExpr Text ElmExpr
  | ElmMonOp Text ElmExpr
  | ElmTernOp ElmExpr ElmExpr ElmExpr
  | ElmCaseExpr ElmExpr [(ElmPattern, ElmExpr)]
  | ElmLetIn ElmPattern ElmExpr ElmExpr
  | ElmMember ElmExpr Text
  | ElmStringExpr Text
  | ElmTupleExpr [ElmExpr]
  | ElmListExpr [ElmExpr]
  | ElmFuncAppl ElmExpr [ElmExpr]
  | ElmRecord [(Text, ElmExpr)]
  | ElmRecordUpdate Text [(Text, ElmExpr)]
  | ElmLambda [ElmPattern] ElmExpr
  | ElmParenExpr ElmExpr
  deriving (Eq, Ord)

instance IsString ElmExpr where
  fromString = ElmExprIdent . fromString

instance Num ElmExpr where
  fromInteger = ElmExprInt . fromInteger
  (+)    = error "Num instance only for fromInteger"
  (*)    = error "Num instance only for fromInteger"
  abs    = error "Num instance only for fromInteger"
  signum = error "Num instance only for fromInteger"
  negate = error "Num instance only for fromInteger"

data ElmStmt
  = ElmModule Text [Text]
  | ElmImport Text [Text]
  | ElmTypeSig Text ElmType
  | ElmDef ElmPattern ElmExpr
  | ElmTypeDef Text [(Text, [ElmType])]
  | ElmTypeAlias Text ElmType
  | ElmComment ElmStmt Text
  | ElmBlankLine
  | ElmStmts [ElmStmt]
  deriving (Eq, Ord)

