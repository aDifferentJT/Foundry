{-# LANGUAGE RecordWildCards, TupleSections #-}

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

import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import System.Environment (lookupEnv)
import System.FilePath ((-<.>), takeBaseName, takeDirectory)
import System.IO (Handle, hPutStrLn, hFlush)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)

import Assembler.C.AST
import Proc
import Utils

import Paths_Foundry

genStringIn :: CExpr e => e -> [String] -> CExprRaw
genStringIn x []  = AnyCExpr (0 :: Int)
genStringIn x [y] = CBinOp (cFuncCall "strcmp" x (CString y) :: CFuncCall) "==" (0 :: Int)
genStringIn x ys  = AnyCExpr (cFuncCall "string_in" x $$ map CString ys $ "NULL" :: CFuncCall)

genIncludes :: CStmts
genIncludes = cStmts
  (CLocalInclude "assembler.h")
  (CLocalInclude "plugin.h")
  (CLocalInclude "utils.h")
  CBlankLine
  (CGlobalInclude "stdlib.h")
  (CGlobalInclude "string.h")

genParseInst :: [Inst] -> CFuncDef
genParseInst insts = CFuncDef
  "inst_error_t"
  "parseInst"
  [(CPtr "char", "str"), (CPtr "inst_t", "inst")]
  ( CBlock $ cStmts
    (CDecl (CPtr "char") "instName" (Just (cFuncCall "strtok_r" "str" (CString " ") (CMonOp "&" "str") :: CFuncCall)))
    (CAssign (CArrow "inst" "name") (CFuncCall "strdup" ["instName"]))
    (CAssign (CArrow "inst" "width") (8 :: Int))
    ( CIf
      [ let n = length args in
        ( genStringIn "instName" (map (\(Inst i _ _ _) -> i) insts)
        , CBlock . CStmts $
          [ CAssign (CArrow "inst" "arg_count") n
          , CAssign (CArrow "inst" "args") (case n of
              0 -> AnyCExpr "NULL"
              1 -> AnyCExpr (CFuncCall "malloc" [CFuncCall "sizeof" ["arg_t"]])
              _ -> AnyCExpr (CFuncCall "malloc" [CBinOp (length args) "*" (CFuncCall "sizeof" ["arg_t"])])
            )
          ] ++
          [ case arg of
              RegT w  -> CTopExpr (cFuncCall "parseRegArg"  (CMonOp "&" (CIndex (CArrow "inst" "args") i)) w "str" (CMonOp "&" "str") :: CFuncCall)
              BitsT w -> CTopExpr (cFuncCall "parseBitsArg" (CMonOp "&" (CIndex (CArrow "inst" "args") i)) w "str" (CMonOp "&" "str") :: CFuncCall)
              IntT w  -> CTopExpr (cFuncCall "parseIntArg"  (CMonOp "&" (CIndex (CArrow "inst" "args") i)) w "str" (CMonOp "&" "str") :: CFuncCall)
              InstT   -> CComment CBlankLine "Instruction arguments not supported by the assembler"
            | (arg, i) <- zip args [0..]
          ]
        )
        | (args, insts) <- groupWith (\(Inst _ as _ _) -> as) insts
      ]
      ( Just . CBlock $ cStmts
        (CTopExpr (cFuncCall "fprintf" "stderr" (CString "Instruction %s not recognised\n") :: CFuncCall))
        (CReturn "Inst_Not_Exist")
      )
    )
    (CReturn "Inst_Ok")
  )

genBitsEnc :: CExpr e => e -> [Bit] -> CStmtRaw
genBitsEnc dest [] = CNonStmt
genBitsEnc dest bs =
  CTopExpr
    ( cFuncCall "copy_list"
        dest
        (length bs)
        "bit_t"
        $$ map show bs
        :: CFuncCall
    )

genBitsExprEnc :: (CExpr e1, CExpr e2, CExpr e3) => (String -> e1) -> e2 -> Int -> e3 -> BitsExpr -> (CStmtRaw, Int)
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
  "reg_error_t"
  "encReg"
  [(CPtr "char", "reg"), (CPtr "bit_t", "dest"), (AnyCType "int", "width")]
  ( CBlock $ cStmts
    ( CSwitch (genStringIn "reg" (map fst regs'))
      ( CBlock
      . CStmts
      . map
        (\((ident, enc), i) -> cStmts
          (CComment (CCase i) ident)
          (genBitsEnc "dest" enc)
          CBreak
          :: CStmts
        )
      $ zip regs' [1 :: Int ..]
      )
    )
    (CReturn "Reg_Not_Exist")
  )
  where regs' :: [(String, [Bit])]
        regs' = mapMaybe (\(Reg i _ e) -> (i,) <$> e) regs

genEncInst :: [Inst] -> CFuncDef
genEncInst insts = CFuncDef
  (CPtr "bit_t")
  "encInst"
  [(AnyCType "inst_t", "inst"), (CPtr "void", "data")]
  ( CBlock $ cStmts
    (CDecl (CPtr "bit_t") "dest" (Just (CFuncCall "malloc" [CBinOp (CMember "inst" "width") "*" (CFuncCall "sizeof" ["bit_t"])])))
    ( CSwitch (genStringIn (CMember "inst" "name") [i | (Inst i _ _ _) <- insts])
      ( CBlock
      . CStmts
      . map
        (\(Inst ident _ _ (argNames, (bs, enc)), i) -> cStmts
          (CComment (CCase i) ident)
          (genBitsEnc "dest" bs)
          (fst . genBitsExprEnc (CIndex (CMember "inst" "args") . fromJust . flip elemIndex argNames) "dest" (length bs) "data" $ enc)
          CBreak
          :: CStmts
        )
      $ zip insts [1 :: Int ..]
      )
    )
    (CReturn "dest")
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

