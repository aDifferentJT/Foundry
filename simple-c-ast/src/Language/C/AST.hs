{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs, NoImplicitPrelude, OverloadedStrings, RankNTypes #-}

{-|
Module      : Language.C.AST
Description : A simple C AST
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.C.AST
  where

import ClassyPrelude
import Maps.List (mapHead, mapLast)

import qualified Data.Text as Text

joinTailsToHeads :: Monoid m => m -> [[m]] -> [m]
joinTailsToHeads _ []         = []
joinTailsToHeads _ [x]        = x
joinTailsToHeads y (x1:x2:xs) =
  mapLast ((++ (fromMaybe mempty . headMay $ x2)) . (++ y)) x1
  ++ joinTailsToHeads y ((fromMaybe mempty . tailMay $ x2) : xs)

type CIdent = Text

data CType
  = CTypeIdent Text
  | CPtr CType

instance IsString CType where
  fromString = CTypeIdent . fromString

data CExpr
  = CExprIdent Text
  | CExprInt Int
  | CParen CExpr
  | CBinOp CExpr CIdent CExpr
  | CMonOp CIdent CExpr
  | CTernOp CExpr CExpr CExpr
  | CMember CExpr CIdent
  | CArrow CExpr CIdent
  | CString Text
  | CIndex CExpr Int
  | CFuncCall CIdent [CExpr]

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
  | CDecl CType CIdent (Maybe CExpr)
  | CIf [(CExpr, CStmt)] (Maybe CStmt)
  | CSwitch CExpr CStmt
  | CCase CExpr
  | CReturn CExpr
  | CBreak
  | CContinue 
  | CFuncDef CType CIdent [(CType, CIdent)] CStmt
  | CStmts [CStmt]
  | CBlock [CStmt]
  | CComment CStmt Text
  | CBlankLine
  | CNonStmt

-- MARK: PRETTY

class Pretty a where
  pretty :: a -> Text
  pretty = intercalate "\n" . prettyLines

  prettyLines :: a -> [Text]
  prettyLines = Text.splitOn "\n" . pretty

instance Pretty CType where
  pretty (CTypeIdent i) = i
  pretty (CPtr t)       = pretty t ++ "*"

instance Pretty CIdent where
  pretty = id

instance Pretty Int where
  pretty = tshow

instance Pretty CExpr where
  pretty (CExprIdent i)    = i
  pretty (CExprInt n)      = tshow n
  pretty (CParen e)        = "(" ++ pretty e ++ ")"
  pretty (CBinOp e1 o e2)  = pretty e1 ++ " " ++ o ++ " " ++ pretty e2
  pretty (CMonOp o e)      = o ++ pretty e
  pretty (CTernOp c e1 e2) = pretty c ++ " ? " ++ pretty e1 ++ " : " ++ pretty e2
  pretty (CMember e i)     = pretty e ++ "." ++ i
  pretty (CArrow e i)      = pretty e ++ "->" ++ i
  pretty (CString s)       = tshow s
  pretty (CIndex e i)      = pretty e ++ "[" ++ tshow i ++ "]"
  pretty (CFuncCall i es)  = i ++ "(" ++ (intercalate ", " . map pretty $ es) ++ ")"

instance Pretty CStmt where
  prettyLines (CLocalInclude  s)  = ["#include \"" ++ s ++ "\""]
  prettyLines (CGlobalInclude s)  = ["#include <" ++ s ++ ">"]
  prettyLines (CDefine s e)       = ["#define " ++ s ++ " " ++ pretty e]
  prettyLines (CUndefine s)       = ["#undef " ++ s]
  prettyLines (CAssign e1 e2)     = [pretty e1 ++ " = " ++ pretty e2 ++ ";"]
  prettyLines (CTopExpr e)        = [pretty e ++ ";"]
  prettyLines (CDecl t i e)       = [pretty t ++ " " ++ i ++ maybe "" ((" = " ++) . pretty) e ++ ";"]
  prettyLines (CIf is e)
    = joinTailsToHeads " else "
      ( map (\(c, b) -> mapHead (("if " ++) . (pretty (CParen c) ++) . (" " ++)) (prettyLines b)) is
      ++ maybe [] ((:[]) . prettyLines) e
      )
  prettyLines (CSwitch c b)       = mapHead (("switch " ++) . (pretty (CParen c) ++) . (" " ++)) (prettyLines b)
  prettyLines (CCase e)           = ["case " ++ pretty e ++ ":"]
  prettyLines (CReturn e)         = ["return " ++ pretty e ++ ";"]
  prettyLines  CBreak             = ["break;"]
  prettyLines  CContinue          = ["continue;"]
  prettyLines (CFuncDef t i as b) =
    mapHead
      ( (pretty t ++)
      . (" " ++)
      . (i ++)
      . ("(" ++)
      . (intercalate ", " [pretty at ++ " " ++ ai | (at, ai) <- as] ++)
      . (") " ++)
      )
      (prettyLines b)
  prettyLines (CStmts ss)         = concatMap prettyLines ss
  prettyLines (CBlock ss)         = ["{"] ++ (map ("  " ++) . concatMap prettyLines $ ss) ++ ["}"]
  prettyLines (CComment s c)      =
    let ls = prettyLines s in
    let n = maximum . ncons 0 . map length $ ls in
    [l ++ Text.replicate (n - length l) " " ++ " // " ++ c | l <- ls]
  prettyLines  CBlankLine         = [""]
  prettyLines  CNonStmt           = []

