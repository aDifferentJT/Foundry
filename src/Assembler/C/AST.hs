{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs, NoImplicitPrelude, OverloadedStrings, RankNTypes #-}

{-|
Module      : Assembler.C.AST
Description : A simple C AST
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Assembler.C.AST
  where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as Text

import ClassyPrelude
import Utils (mapHead, joinTailsToHeads)

type CIdent = Text

class Pretty a => CType a

data CTypeRaw where
  CPtr     :: CType t => t -> CTypeRaw
  AnyCType :: CType t => t -> CTypeRaw

instance CType CTypeRaw
instance CType CIdent

class Pretty a => CExpr a

data CExprRaw where
  CParen   :: CExpr e => e -> CExprRaw
  CBinOp   :: (CExpr e1, CExpr e2) => e1 -> CIdent -> e2 -> CExprRaw
  CMonOp   :: CExpr e => CIdent -> e -> CExprRaw
  CTernOp  :: (CExpr c, CExpr e1, CExpr e2) => c -> e1 -> e2 -> CExprRaw
  CMember  :: CExpr e => e -> CIdent -> CExprRaw
  CArrow   :: CExpr e => e -> CIdent -> CExprRaw
  CString  :: Text -> CExprRaw
  CIndex   :: CExpr e => e -> Int -> CExprRaw
  AnyCExpr :: CExpr e => e -> CExprRaw

class CFuncCallBuilder a where
  cFuncCallRevArgList :: [CExprRaw] -> CIdent -> a

data CFuncCall where
  CFuncCall :: CExpr e => CIdent -> [e] -> CFuncCall

instance CFuncCallBuilder CFuncCall where
  cFuncCallRevArgList as i = CFuncCall i (reverse as)

instance (CFuncCallBuilder a, CExpr e) => CFuncCallBuilder (e -> a) where
  cFuncCallRevArgList as i e = cFuncCallRevArgList (AnyCExpr e : as) i

cFuncCall :: CFuncCallBuilder a => CIdent -> a
cFuncCall = cFuncCallRevArgList []

($$) :: CExpr e => (forall a . CFuncCallBuilder a => a) -> [e] -> (forall a . CFuncCallBuilder a => a)
b $$  []    = b
b $$ (x:xs) = b x $$ xs

instance CExpr CExprRaw
instance CExpr CFuncCall
instance CExpr CIdent
instance CExpr Int

infixr 5 !:

(!:) :: CExpr e => e -> [CExprRaw] -> [CExprRaw]
e !: es = AnyCExpr e : es

class Pretty a => CStmt a

data CStmtRaw where
  CLocalInclude  :: Text -> CStmtRaw
  CGlobalInclude :: Text -> CStmtRaw
  CDefine        :: CExpr e => Text -> e -> CStmtRaw
  CUndefine      :: Text -> CStmtRaw
  CAssign        :: (CExpr e1, CExpr e2) => e1 -> e2 -> CStmtRaw
  CTopExpr       :: CExpr e => e -> CStmtRaw
  CDecl          :: (CType t, CExpr e) => t -> CIdent -> Maybe e -> CStmtRaw
  CIf            :: CExpr e => [(e, CBlock)] -> Maybe CBlock -> CStmtRaw
  CSwitch        :: CExpr e => e -> CBlock -> CStmtRaw
  CCase          :: CExpr e => e -> CStmtRaw
  CReturn        :: CExpr e => e -> CStmtRaw
  CBreak         :: CStmtRaw
  CContinue      :: CStmtRaw
  CComment       :: CStmt s => s -> Text -> CStmtRaw
  CBlankLine     :: CStmtRaw
  CNonStmt       :: CStmtRaw
  AnyCStmt       :: CStmt s => s -> CStmtRaw

data CStmts where
  CStmts :: CStmt s => [s] -> CStmts

newtype CBlock = CBlock CStmts

data CFuncDef where
  CFuncDef :: (CType t1, CType t2) => t1 -> CIdent -> [(t2, CIdent)] -> CBlock -> CFuncDef

instance CStmt CStmtRaw
instance CStmt CStmts
instance CStmt CBlock
instance CStmt CFuncDef

class CStmtsBuilder a where
  cStmtsRev :: [CStmtRaw] -> a

instance CStmtsBuilder CStmts where
  cStmtsRev = CStmts . reverse

instance (CStmtsBuilder a, CStmt s) => CStmtsBuilder (s -> a) where
  cStmtsRev ss s = cStmtsRev (AnyCStmt s : ss)

cStmts :: CStmtsBuilder a => a
cStmts = cStmtsRev []

-- MARK: PRETTY

class Pretty a where
  pretty :: a -> Text
  pretty = intercalate "\n" . prettyLines

  prettyLines :: a -> [Text]
  prettyLines = Text.splitOn "\n" . pretty

instance Pretty CTypeRaw where
  pretty (CPtr t)     = pretty t ++ "*"
  pretty (AnyCType t) = pretty t

instance Pretty CIdent where
  pretty = id

instance Pretty Int where
  pretty = tshow

instance Pretty CExprRaw where
  pretty (CParen e)        = "(" ++ pretty e ++ ")"
  pretty (CBinOp e1 o e2)  = pretty e1 ++ " " ++ o ++ " " ++ pretty e2
  pretty (CMonOp o e)      = o ++ pretty e
  pretty (CTernOp c e1 e2) = pretty c ++ " ? " ++ pretty e1 ++ " : " ++ pretty e2
  pretty (CMember e i)     = pretty e ++ "." ++ i
  pretty (CArrow e i)      = pretty e ++ "->" ++ i
  pretty (CString s)       = tshow s
  pretty (CIndex e i)      = pretty e ++ "[" ++ tshow i ++ "]"
  pretty (AnyCExpr e)      = pretty e

instance Pretty CFuncCall where
  pretty (CFuncCall i es)  = i ++ "(" ++ (intercalate ", " . map pretty $ es) ++ ")"

instance Pretty CStmtRaw where
  prettyLines (CLocalInclude  s) = ["#include \"" ++ s ++ "\""]
  prettyLines (CGlobalInclude s) = ["#include <" ++ s ++ ">"]
  prettyLines (CDefine s e)      = ["#define " ++ s ++ " " ++ pretty e]
  prettyLines (CUndefine s)      = ["#undef " ++ s]
  prettyLines (CAssign e1 e2)    = [pretty e1 ++ " = " ++ pretty e2 ++ ";"]
  prettyLines (CTopExpr e)       = [pretty e ++ ";"]
  prettyLines (CDecl t i e)      = [pretty t ++ " " ++ i ++ maybe "" ((" = " ++) . pretty) e ++ ";"]
  prettyLines (CIf is e)
    = joinTailsToHeads " else "
      ( map (\(c, b) -> mapHead (("if " ++) . (pretty (CParen c) ++) . (" " ++)) (prettyLines b)) is
      ++ maybe [] ((:[]) . prettyLines) e
      )
  prettyLines (CSwitch c b)      = mapHead (("switch " ++) . (pretty (CParen c) ++) . (" " ++)) (prettyLines b)
  prettyLines (CCase e)          = ["case " ++ pretty e ++ ":"]
  prettyLines (CReturn e)        = ["return " ++ pretty e ++ ";"]
  prettyLines  CBreak            = ["break;"]
  prettyLines  CContinue         = ["continue;"]
  prettyLines (CComment s c)     =
    let ls = prettyLines s in
    let n = maximum . ncons 0 . map length $ ls in
    [l ++ Text.replicate (n - length l) " " ++ " // " ++ c | l <- ls]
  prettyLines  CBlankLine        = [""]
  prettyLines  CNonStmt          = []
  prettyLines (AnyCStmt s)       = prettyLines s

instance Pretty CStmts where
  prettyLines (CStmts ss) = concatMap prettyLines ss

instance Pretty CBlock where
  prettyLines (CBlock ss) = ["{"] ++ (map ("  " ++) . prettyLines $ ss) ++ ["}"]

instance Pretty CFuncDef where
  prettyLines (CFuncDef t i as b) =
    mapHead
      ( (pretty t ++)
      . (" " ++)
      . (i ++)
      . ("(" ++)
      . (intercalate ", " [pretty t ++ " " ++ i | (t, i) <- as] ++)
      . (") " ++)
      )
      (prettyLines b)

