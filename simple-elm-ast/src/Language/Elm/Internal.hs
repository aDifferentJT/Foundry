{-# LANGUAGE ExistentialQuantification, FlexibleInstances, NoImplicitPrelude, OverloadedStrings, RankNTypes, TupleSections #-}

{-|
Module      : Language.Elm.Internal
Description : Internals for elm
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.Elm.Internal
  ( ElmAssoc(..)
  , ElmPrec(..)
  , precOfOp
  , ElmIndent(..)
  ) where

import ClassyPrelude

data ElmAssoc = ElmLeftAssoc | ElmRightAssoc | ElmNoAssoc

instance Eq ElmAssoc where
  ElmLeftAssoc  == ElmLeftAssoc  = True
  ElmRightAssoc == ElmRightAssoc = True
  _             == _             = False

data ElmPrec
  = ElmPrecNone
  | ElmPrec0
  | ElmPrec1
  | ElmPrec2
  | ElmPrec3
  | ElmPrec4
  | ElmPrec5
  | ElmPrec6
  | ElmPrec7
  | ElmPrec8
  | ElmPrec9
  | ElmPrecFuncAppl
  | ElmPrecMember
  | ElmPrecLit
  | ElmPrecParen
  deriving (Eq, Ord)

precOfOp :: Text -> Maybe (ElmAssoc, ElmPrec)
precOfOp ">>" = Just (ElmLeftAssoc,  ElmPrec9)
precOfOp "<<" = Just (ElmRightAssoc, ElmPrec9)
precOfOp "^"  = Just (ElmRightAssoc, ElmPrec8)
precOfOp "*"  = Just (ElmLeftAssoc,  ElmPrec7)
precOfOp "/"  = Just (ElmLeftAssoc,  ElmPrec7)
precOfOp "//" = Just (ElmLeftAssoc,  ElmPrec7)
precOfOp "+"  = Just (ElmLeftAssoc,  ElmPrec6)
precOfOp "-"  = Just (ElmLeftAssoc,  ElmPrec6)
precOfOp "++" = Just (ElmRightAssoc, ElmPrec5)
precOfOp "::" = Just (ElmRightAssoc, ElmPrec5)
precOfOp "==" = Just (ElmNoAssoc,    ElmPrec4)
precOfOp "/=" = Just (ElmNoAssoc,    ElmPrec4)
precOfOp "<"  = Just (ElmNoAssoc,    ElmPrec4)
precOfOp ">"  = Just (ElmNoAssoc,    ElmPrec4)
precOfOp "<=" = Just (ElmNoAssoc,    ElmPrec4)
precOfOp ">=" = Just (ElmNoAssoc,    ElmPrec4)
precOfOp "&&" = Just (ElmRightAssoc, ElmPrec3)
precOfOp "||" = Just (ElmRightAssoc, ElmPrec2)
precOfOp "|>" = Just (ElmLeftAssoc,  ElmPrec0)
precOfOp "<|" = Just (ElmRightAssoc, ElmPrec0)
precOfOp _    = Nothing

data ElmIndent
  = ElmNoIndent
  | ElmIndentBy Int ElmIndent
  | ElmIndentToMul Int ElmIndent

