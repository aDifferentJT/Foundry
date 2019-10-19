{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, TupleSections #-}

{-|
Module      : Assembler.GenAssembler
Description : Generate an assembler
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Assembler.GenAssembler
  ( genAssembler
  ) where

import ClassyPrelude

import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Text.IO (hPutStrLn)
import System.Environment (lookupEnv)
import System.FilePath ((-<.>), takeBaseName, takeDirectory)
import System.IO (Handle)
import System.Process (callProcess)

import Assembler.C.AST
import Proc
import Utils

import Paths_Foundry

genStringIn :: CExpr e => e -> [Text] -> CExprRaw
genStringIn x []  = AnyCExpr (0 :: Int)
genStringIn x [y] = CBinOp (cFuncCall ("strcmp" :: CIdent) x (CString y) :: CFuncCall) ("==" :: CIdent) (0 :: Int)
genStringIn x ys  = AnyCExpr (cFuncCall ("string_in" :: CIdent) x $$ map CString ys $ ("NULL" :: CIdent) :: CFuncCall)

genIncludes :: CStmts
genIncludes = cStmts
  (CLocalInclude ("assembler.h" :: CIdent))
  (CLocalInclude ("plugin.h" :: CIdent))
  (CLocalInclude ("utils.h" :: CIdent))
  CBlankLine
  (CGlobalInclude ("stdlib.h" :: CIdent))
  (CGlobalInclude ("string.h" :: CIdent))

genParseInst :: [Inst] -> CFuncDef
genParseInst insts = CFuncDef
  ("inst_error_t" :: CIdent)
  ("parseInst" :: CIdent)
  [(CPtr ("char" :: CIdent), "str" :: CIdent), (CPtr ("inst_t" :: CIdent), "inst" :: CIdent)]
  ( CBlock $ cStmts
    (CDecl (CPtr ("char" :: CIdent)) ("instName" :: CIdent) (Just (cFuncCall ("strtok_r" :: CIdent) ("str" :: CIdent) (CString " ") (CMonOp ("&" :: CIdent) ("str" :: CIdent)) :: CFuncCall)))
    (CAssign (CArrow ("inst" :: CIdent) ("name" :: CIdent)) (CFuncCall ("strdup" :: CIdent) ["instName" :: CIdent]))
    (CAssign (CArrow ("inst" :: CIdent) ("width" :: CIdent)) (8 :: Int))
    ( CIf
      [ let n = length args in
        ( genStringIn ("instName" :: CIdent) (map (\(Inst i _ _ _) -> i) insts)
        , CBlock . CStmts $
          [ CAssign (CArrow ("inst" :: CIdent) ("arg_count" :: CIdent)) n
          , CAssign (CArrow ("inst" :: CIdent) ("args" :: CIdent)) (case n of
              0 -> AnyCExpr ("NULL" :: CIdent)
              1 -> AnyCExpr (CFuncCall ("malloc" :: CIdent) [CFuncCall ("sizeof" :: CIdent) ["arg_t" :: CIdent]])
              _ -> AnyCExpr (CFuncCall ("malloc" :: CIdent) [CBinOp (length args) ("*" :: CIdent) (CFuncCall ("sizeof" :: CIdent) ["arg_t" :: CIdent])])
            )
          ] ++
          [ case arg of
              RegT w  -> CTopExpr (cFuncCall ("parseRegArg" :: CIdent)  (CMonOp ("&" :: CIdent) (CIndex (CArrow ("inst" :: CIdent) ("args" :: CIdent)) i)) w ("str" :: CIdent) (CMonOp ("&" :: CIdent) ("str" :: CIdent)) :: CFuncCall)
              BitsT w -> CTopExpr (cFuncCall ("parseBitsArg" :: CIdent) (CMonOp ("&" :: CIdent) (CIndex (CArrow ("inst" :: CIdent) ("args" :: CIdent)) i)) w ("str" :: CIdent) (CMonOp ("&" :: CIdent) ("str" :: CIdent)) :: CFuncCall)
              IntT w  -> CTopExpr (cFuncCall ("parseIntArg" :: CIdent)  (CMonOp ("&" :: CIdent) (CIndex (CArrow ("inst" :: CIdent) ("args" :: CIdent)) i)) w ("str" :: CIdent) (CMonOp ("&" :: CIdent) ("str" :: CIdent)) :: CFuncCall)
              InstT   -> CComment CBlankLine "Instruction arguments not supported by the assembler"
            | (arg, i) <- zip args [0..]
          ]
        )
        | (args, insts) <- groupWith (\(Inst _ as _ _) -> as) insts
      ]
      ( Just . CBlock $ cStmts
        (CTopExpr (cFuncCall ("fprintf" :: CIdent) ("stderr" :: CIdent) (CString "Instruction %s not recognised\n") :: CFuncCall))
        (CReturn ("Inst_Not_Exist" :: CIdent))
      )
    )
    (CReturn ("Inst_Ok" :: CIdent))
  )

genBitsEnc :: CExpr e => e -> [Bit] -> CStmtRaw
genBitsEnc dest [] = CNonStmt
genBitsEnc dest bs =
  CTopExpr
    ( cFuncCall
        ("copy_list" :: CIdent)
        dest
        (length bs)
        ("bit_t" :: CIdent)
        $$ map tshow bs
        :: CFuncCall
    )

genBitsExprEnc :: (CExpr e1, CExpr e2, CExpr e3) => (Text -> e1) -> e2 -> Int -> e3 -> BitsExpr -> (CStmtRaw, Int)
genBitsExprEnc getArg dest offset auxData (ConstBitsExpr bs) =
  ( genBitsEnc dest bs
  , offset + length bs
  )
genBitsExprEnc getArg dest offset auxData (EncBitsExpr w i) =
  ( CTopExpr (cFuncCall "encArg" (getArg i) (CBinOp dest "+" offset) auxData :: CFuncCall)
  , offset + w
  )
genBitsExprEnc getArg dest offset auxData (ConcatBitsExpr _ xs ys) =
  let (xStmt, xOff) = genBitsExprEnc getArg dest offset auxData xs in
  let (yStmt, yOff) = genBitsExprEnc getArg dest xOff auxData ys in
  (AnyCStmt . CStmts $ [xStmt, yStmt], yOff)

genEncReg :: [Reg] -> CFuncDef
genEncReg regs = CFuncDef
  ("reg_error_t" :: CIdent)
  ("encReg" :: CIdent)
  [(CPtr ("char" :: CIdent), "reg" :: CIdent), (CPtr ("bit_t" :: CIdent), "dest" :: CIdent), (AnyCType ("int" :: CIdent), "width" :: CIdent)]
  ( CBlock $ cStmts
    ( CSwitch (genStringIn ("reg" :: CIdent) (map fst regs'))
      ( CBlock
      . CStmts
      . map
        (\((ident, enc), i) -> cStmts
          (CComment (CCase i) ident)
          (genBitsEnc ("dest" :: CIdent) enc)
          CBreak
          :: CStmts
        )
      $ zip regs' [1 :: Int ..]
      )
    )
    (CReturn ("Reg_Not_Exist" :: CIdent))
  )
  where regs' :: [(Text, [Bit])]
        regs' = mapMaybe (\(Reg i _ e) -> (i,) <$> e) regs

genEncInst :: [Inst] -> CFuncDef
genEncInst insts = CFuncDef
  (CPtr ("bit_t" :: CIdent))
  ("encInst" :: CIdent)
  [(AnyCType ("inst_t" :: CIdent), "inst" :: CIdent), (CPtr ("void" :: CIdent), "data" :: CIdent)]
  ( CBlock $ cStmts
    (CDecl (CPtr ("bit_t" :: CIdent)) ("dest" :: CIdent) (Just (CFuncCall ("malloc" :: CIdent) [CBinOp (CMember ("inst" :: CIdent) ("width" :: CIdent)) ("*" :: CIdent) (CFuncCall ("sizeof" :: CIdent) ["bit_t" :: CIdent])])))
    ( CSwitch (genStringIn (CMember ("inst" :: CIdent) ("name" :: CIdent)) [i | (Inst i _ _ _) <- insts])
      ( CBlock
      . CStmts
      . map
        (\(Inst ident _ _ (argNames, (bs, enc)), i) -> cStmts
          (CComment (CCase i) ident)
          (genBitsEnc ("dest" :: CIdent) bs)
          (fst . genBitsExprEnc (CIndex (CMember ("inst" :: CIdent) ("args" :: CIdent)) . fromJust . flip elemIndex argNames) ("dest" :: CIdent) (length bs) ("data" :: CIdent) $ enc)
          CBreak
          :: CStmts
        )
      $ zip insts [1 :: Int ..]
      )
    )
    (CReturn ("dest" :: CIdent))
  )

genPlugin :: Proc -> CStmts
genPlugin Proc{..} = cStmts
  genIncludes
  CBlankLine
  (genParseInst insts)
  CBlankLine
  (genEncReg regs)
  CBlankLine
  (genEncInst insts)

writePlugin :: Handle -> Proc -> IO ()
writePlugin h = hPutStrLn h . pretty . genPlugin

compilePlugin :: FilePath -> FilePath -> IO ()
compilePlugin pluginFn binFn = do
  includeDirs <- mapM (fmap takeDirectory . getDataFileName)
    [ "assembler/assembler.h"
    , "assembler/plugin.h"
    , "assembler/utils.h"
    ]
  assemblerC <- getDataFileName "assembler/assembler.c"
  utilsC <- getDataFileName "assembler/utils.c"
  cc <- fromMaybe "cc" <$> lookupEnv "CC"
  callProcess cc (concatMap (("-I":) . (:[])) includeDirs ++ ["-o", binFn, assemblerC, utilsC, pluginFn])

genAssembler :: FilePath -> Proc -> IO ()
genAssembler fn ast = withSystemTempFile (takeBaseName fn -<.> ".c") $ \tmpFn tmpH -> do
  writePlugin tmpH ast
  hFlush tmpH
  compilePlugin tmpFn fn

