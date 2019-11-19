{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, TupleSections #-}

{-|
Module      : GenAssembler
Description : Generate an assembler
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module GenAssembler
  ( genAssembler
  ) where

import ClassyPrelude

import Data.Bit (Bit)
import Data.List.Group (groupWith)
import Language.C.AST
import Language.C.Pretty (pretty)
import Language.Foundry.Proc

import Paths_foundry

import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Text.IO (hPutStrLn)
import System.Environment (lookupEnv)
import System.FilePath ((-<.>), takeBaseName)
import System.IO (Handle)
import System.Process (callProcess)

genStringIn :: CExpr -> [Text] -> CExpr
genStringIn _ []  = 0
genStringIn x [y] = CBinOp (CFuncCall "strcmp" [x, CString y]) "==" 0
genStringIn x ys  = CFuncCall "string_in" $ [x] ++ map CString ys ++ ["NULL"]

genIncludes :: CStmt
genIncludes = CStmts
  [ CLocalInclude "assembler.h"
  , CLocalInclude "plugin.h"
  , CLocalInclude "utils.h"
  , CBlankLine
  , CGlobalInclude "stdlib.h"
  , CGlobalInclude "string.h"
  ]

genParseInst :: Proc -> CStmt
genParseInst Proc{..} = CFuncDef
  "inst_error_t"
  "parseInst"
  [(CPtr "char", "str"), (CPtr "inst_t", "inst")]
  . CBlock
  $ [ CDecl (CPtr "char") "instName" . Just . CFuncCall "strtok_r" $ ["str", CString " ", CMonOp "&" "str"]
    , CAssign (CArrow "inst" "name") $ CFuncCall "strdup" ["instName"]
    , CAssign (CArrow "inst" "width") 8
    , CIf
      [ let n = length args in
        ( genStringIn "instName" (map (\(Inst i _ _ _) -> i) insts')
        , CBlock $
          [ CAssign (CArrow "inst" "arg_count") (CExprInt n)
          , CAssign (CArrow "inst" "args") (case n of
              0 -> "NULL"
              1 -> CFuncCall "malloc" [CFuncCall "sizeof" ["arg_t"]]
              _ -> CFuncCall "malloc" [CBinOp (CExprInt . length $ args) "*" (CFuncCall "sizeof" ["arg_t"])]
            )
          ] ++
          [ case arg of
              RegT w  ->
                CTopExpr
                . CFuncCall "parseRegArg"
                $ [ CMonOp "&" (CIndex (CArrow "inst" "args") i)
                  , CExprInt w
                  , "str"
                  , CMonOp "&" "str"
                  ]
              BitsT w ->
                CTopExpr
                . CFuncCall "parseBitsArg"
                $ [ CMonOp "&" (CIndex (CArrow "inst" "args") i)
                  , CExprInt w
                  , "str"
                  , CMonOp "&" "str"
                  ]
              IntT w  ->
                CTopExpr
                . CFuncCall "parseIntArg"
                $ [ CMonOp "&" (CIndex (CArrow "inst" "args") i)
                  , CExprInt w
                  , "str"
                  , CMonOp "&" "str"
                  ]
              InstT   -> CComment CBlankLine "Instruction arguments not supported by the assembler"
            | (arg, i) <- zip args [0..]
          ]
        )
        | (args, insts') <- groupWith (\(Inst _ as _ _) -> as) insts
      ]
      ( Just . CBlock $
        [ CTopExpr (CFuncCall "fprintf" ["stderr", CString "Instruction %s not recognised\n"])
        , CReturn "Inst_Not_Exist"
        ]
      )
    , CReturn "Inst_Ok"
    ]

genBitsEnc :: CExpr -> [Bit] -> CStmt
genBitsEnc _    [] = CNonStmt
genBitsEnc dest bs =
  CTopExpr
  . CFuncCall "copy_list"
  $ [ dest
    , CExprInt . length $ bs
    , "bit_t"
    ] ++ map (CExprIdent . tshow) bs

genBitsExprEnc :: (Text -> CExpr) -> CExpr -> Int -> CExpr -> BitsExpr -> (CStmt, Int)
genBitsExprEnc _      dest offset _       (ConstBitsExpr bs) =
  ( genBitsEnc dest bs
  , offset + length bs
  )
genBitsExprEnc getArg dest offset auxData (EncBitsExpr w i) =
  ( CTopExpr
    . CFuncCall "encArg"
    $ [ getArg i
      , CBinOp dest "+" (CExprInt offset)
      , auxData
      ]
  , offset + w
  )
genBitsExprEnc getArg dest offset auxData (ConcatBitsExpr _ xs ys) =
  let (xStmt, xOff) = genBitsExprEnc getArg dest offset auxData xs in
  let (yStmt, yOff) = genBitsExprEnc getArg dest xOff auxData ys in
  (CStmts [xStmt, yStmt], yOff)

genEncReg :: Proc -> CStmt
genEncReg Proc{..} = CFuncDef
  "reg_error_t"
  "encReg"
  [(CPtr "char", "reg"), (CPtr "bit_t", "dest"), ("int", "width")]
  . CBlock
  $ [ CSwitch (genStringIn "reg" (map fst regs'))
      . CBlock
      . map
        (\((ident, enc), i) -> CStmts
          [ CComment (CCase . CExprInt $ i) ident
          , genBitsEnc "dest" enc
          , CBreak
          ]
        )
      $ zip regs' [1 :: Int ..]
    , CReturn "Reg_Not_Exist"
    ]
  where regs' :: [(Text, [Bit])]
        regs' = mapMaybe (\(Reg i _ e) -> (i,) <$> e) regs

genEncInst :: Proc -> CStmt
genEncInst Proc{..} = CFuncDef
  (CPtr "bit_t")
  "encInst"
  [("inst_t", "inst"), (CPtr "void", "data")]
  . CBlock
  $ [ CDecl (CPtr "bit_t") "dest" (Just (CFuncCall "malloc" [CBinOp (CMember "inst" "width") "*" (CFuncCall "sizeof" ["bit_t"])]))
    , CSwitch (genStringIn (CMember "inst" "name") [i | (Inst i _ _ _) <- insts])
      ( CBlock
      . map
        (\(Inst ident _ _ (argNames, (bs, enc)), i) -> CStmts
          [ CComment (CCase . CExprInt $ i) ident
          , genBitsEnc "dest" bs
          , fst . genBitsExprEnc (CIndex (CMember "inst" "args") . fromJust . flip elemIndex argNames) "dest" (length bs) "data" $ enc
          , CBreak
          ]
        )
      $ zip insts [1 :: Int ..]
      )
    , CReturn "dest"
    ]

genPlugin :: Proc -> CStmt
genPlugin p = CStmts
  [ genIncludes
  , CBlankLine
  , genParseInst p
  , CBlankLine
  , genEncReg p
  , CBlankLine
  , genEncInst p
  ]

writePlugin :: Handle -> Proc -> IO ()
writePlugin h = hPutStrLn h . pretty . genPlugin

compile :: FilePath -> FilePath -> IO ()
compile pluginFn binFn = do
  includeDir <- getDataFileName "assembler/"
  assemblerC <- getDataFileName "assembler/assembler.c"
  utilsC <- getDataFileName "assembler/utils.c"
  cc <- fromMaybe "cc" <$> lookupEnv "CC"
  callProcess cc ["-I" ++ includeDir, "-o", binFn, assemblerC, utilsC, pluginFn]

genAssembler :: FilePath -> Proc -> IO ()
genAssembler fn ast = withSystemTempFile (takeBaseName fn -<.> ".c") $ \tmpFn tmpH -> do
  writePlugin tmpH ast
  hFlush tmpH
  compile tmpFn fn

