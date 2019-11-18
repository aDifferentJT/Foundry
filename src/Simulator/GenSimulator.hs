{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

{-|
Module      : Simulator.GenSimulator
Description : Generate a simulator
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Simulator.GenSimulator
  ( genSimulator
  ) where

import ClassyPrelude

import Bits (Bit(..), Endianness(..), bitsToInt)
import Simulator.Elm.AST
import Proc
import Utils (flap, textHeadToUpper)

import Paths_Foundry

import Data.List (foldl)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text.IO (hPutStrLn)
import System.Directory (copyFile, createDirectory, getCurrentDirectory)
import System.FilePath ((</>), takeBaseName)
import System.IO (Handle)
import System.Process (CreateProcess(cwd), createProcess, proc, waitForProcess)

elmType :: Type -> ElmType
elmType (RegT _)  = "String"
elmType (BitsT _) = ElmTupleType []
elmType (IntT n)  = ElmTypeFuncAppl "IntW" [ElmTypeIdent $ "Num" ++ tshow n]
elmType  InstT    = ElmTupleType []

elmOp :: Op -> ElmExpr
elmOp Add        = ElmParenExpr "+"
elmOp Sub        = ElmParenExpr "-"
elmOp Mul        = ElmParenExpr "*"
elmOp Div        = ElmParenExpr "/"
elmOp ConcatBits = "concatBits"
elmOp BitwiseAnd = ElmMember "Bitwise" "and"
elmOp BitwiseOr  = ElmMember "Bitwise" "or"
elmOp BitwiseXor = ElmMember "Bitwise" "xor"

elmBoolExpr :: Proc -> BoolExpr -> ElmExpr
elmBoolExpr Proc{..} (EqualityExpr e1 e2)   = ElmBinOp (elmExpr Proc{..} 1 e1) "==" (elmExpr Proc{..} 1 e2)
elmBoolExpr Proc{..} (InequalityExpr e1 e2) = ElmBinOp (elmExpr Proc{..} 1 e1) "/=" (elmExpr Proc{..} 1 e2)
elmBoolExpr Proc{..} (LogicalAndExpr e1 e2) = ElmBinOp (elmBoolExpr Proc{..} e1) "&&" (elmBoolExpr Proc{..} e2)
elmBoolExpr Proc{..} (LogicalOrExpr e1 e2)  = ElmBinOp (elmBoolExpr Proc{..} e1) "||" (elmBoolExpr Proc{..} e2)

elmExpr :: Proc -> Int -> Expr -> ElmExpr
elmExpr Proc{..} _ (VarExpr _ i)         = ElmExprIdent $ "arg_" ++ i
elmExpr Proc{..} _ (RegExpr _ i)         = ElmMember "simState" i
elmExpr Proc{..} _ (MemAccessExpr _ i e) =
  let (dw, aw) =
        fromMaybe (0, 0)
        . headMay
        . mapMaybe (\case
              Memory i' dw' aw'
                | i == i'   -> Just (dw', aw')
                | otherwise -> Nothing
            )
        $ memorys
  in
  ElmFuncAppl "withDefault"
    [ ElmFuncAppl (ElmExprIdent $ "int" ++ tshow dw) [0]
    , ElmFuncAppl (ElmMember "Array" "get")
        [ ElmFuncAppl "toInt" [elmExpr Proc{..} aw e]
        , ElmMember "simState" i
        ]
    ]
elmExpr Proc{..} w (ConstExpr n)         = ElmFuncAppl (ElmExprIdent $ "int" ++ tshow w) [ElmExprInt n]
elmExpr Proc{..} _ (BinaryConstExpr bs)  = ElmFuncAppl (ElmExprIdent $ "int" ++ (tshow . length $ bs)) [ElmExprInt . bitsToInt Little $ bs]
elmExpr Proc{..} w (OpExpr _ o e1 e2)    = ElmFuncAppl "binOpW" [elmOp o, elmExpr Proc{..} w e1, elmExpr Proc{..} w e2]
elmExpr Proc{..} w (TernaryExpr _ c t f) = ElmTernOp (elmBoolExpr Proc{..} c) (elmExpr Proc{..} w t) (elmExpr Proc{..} w f)

elmImplRule :: Proc -> ImplRule -> (Text, ElmExpr)
elmImplRule Proc{..} (ImplRule (VarLValue _) _) = error "Doesn't support register arguments"
elmImplRule Proc{..} (ImplRule (RegLValue i) e) =
  let w =
        fromMaybe 0
        . headMay
        . mapMaybe (\(Reg i' w' _) -> if i == i' then Just w' else Nothing)
        $ regs
  in
  (i, elmExpr Proc{..} w e)
elmImplRule Proc{..} (ImplRule (MemAccessLValue i e1) e2) =
  let w =
        fromMaybe 0
        . headMay
        . mapMaybe (\(Memory i' dw _) -> if i == i' then Just dw else Nothing)
        $ memorys
  in
  ( i
  , ElmFuncAppl (ElmMember "Array" "set")
      [ ElmFuncAppl "toInt" [elmExpr Proc{..} w e1]
      , elmExpr Proc{..} w e2
      , ElmMember "simState" i
      ]
  )

genHeader :: Proc -> ElmStmt
genHeader _ = ElmStmts
  [ ElmModule "Main" ["main"]
  , ElmBlankLine
  , ElmImport "Array" ["Array"]
  , ElmImport "Bitwise" []
  , ElmImport "IntWidths" [".."]
  , ElmImport "Interface" [".."]
  , ElmImport "Maybe" ["withDefault"]
  , ElmImport "Maybe.Extra" []
  , ElmImport "String" []
  ]

genSimState :: Proc -> ElmStmt
genSimState Proc{..} = ElmTypeAlias "SimState" . ElmRecordType
  $   [ ( i
        , elmType . IntT $ w
        )
        | Reg i w _ <- regs
      ]
  ++  [ ( i
        , ElmTypeFuncAppl "Array" [elmType . IntT $ dw]
        )
        | Memory i dw _ <- memorys
      ]

genPerformButton :: Proc -> ElmStmt
genPerformButton Proc{..} = ElmStmts
  [ ElmTypeSig "performButton"
      ( ElmFuncType "Int"
      . ElmFuncType "SimState"
      $ "SimState"
      )
  , ElmDef (ElmPatFuncAppl "performButton" ["n", "simState"])
    . ElmCaseExpr "n"
    $ [ ( ElmPatInt n
        , ElmRecordUpdate "simState" (map (elmImplRule Proc{..}) rs)
        )
        | Button _ n rs <- buttons
      ] ++ [("_", "simState")]
  ]

genGetButtonName :: Proc -> ElmStmt
genGetButtonName Proc{..} = ElmStmts
  [ ElmTypeSig "getButtonName" (ElmFuncType "Int" "String")
  , ElmDef (ElmPatFuncAppl "getButtonName" ["n"])
    . ElmCaseExpr "n"
    $ [ ( ElmPatInt n
        , ElmStringExpr i
        )
        | Button i n _ <- buttons
      ] ++ [("_", ElmFuncAppl (ElmMember "String" "fromInt") ["n"])]
  ]

genInst :: Proc -> ElmStmt
genInst Proc{..} = ElmTypeDef "Inst"
  [ ( textHeadToUpper i
    , map elmType as
    )
    | Inst i as _ _ <- insts
  ]

genShowInst :: Proc -> ElmStmt
genShowInst Proc{..} = ElmStmts
  [ ElmTypeSig "showInst" (ElmFuncType "Inst" "String")
  , ElmDef (ElmPatFuncAppl "showInst" ["i"])
    . ElmCaseExpr "i"
    $ [ ( ElmPatFuncAppl (textHeadToUpper i) (zipWith (\n _ -> ElmPatIdent . ("arg" ++) . tshow $ n) [1 :: Int ..] as)
        , foldl
            (\e (n, a) ->
              ElmBinOp e "++"
              . ElmBinOp (ElmStringExpr " ") "++"
              . showArg a
              . ElmExprIdent
              . ("arg" ++)
              . tshow
              $ n
            )
            (ElmStringExpr i)
          . zip [1 :: Int ..]
          $ as
        )
        | Inst i as _ _ <- insts
      ]
  ]
  where showArg :: Type -> ElmExpr -> ElmExpr
        showArg (RegT _)  e = e
        showArg (BitsT _) e = e
        showArg (IntT _)  e = ElmBinOp (ElmBinOp (ElmMember "String" "fromInt") "<<" "toInt") "<|" e
        showArg  InstT    e = e

genReadInst :: Proc -> ElmStmt
genReadInst Proc{..} = ElmStmts
  [ ElmTypeSig "readInst"
      ( ElmFuncType
          "String"
          (ElmTypeFuncAppl "Maybe" ["Inst"])
      )
  , ElmDef (ElmPatFuncAppl "readInst" ["s"])
    . ElmCaseExpr
        ( ElmBinOp 
            ( ElmBinOp
                (ElmFuncAppl (ElmMember "List" "filter") [ElmBinOp "not" "<<" (ElmMember "String" "isEmpty")])
                "<<"
                (ElmFuncAppl (ElmMember "String" "split") [ElmStringExpr " "])
            )
            "<|"
            "s"
        )
    $ [ ( ElmListPat $ ElmStringPat i : zipWith (\n _ -> ElmPatIdent . ("arg" ++) . tshow $ n) [1 :: Int ..] as
        , foldl
            (\e (n, a) ->
              ElmFuncAppl
                (ElmMember (ElmMember "Maybe" "Extra") "andMap")
                [ readArg a . ElmExprIdent . ("arg" ++) . tshow $ n
                , e
                ]
            )
            (ElmFuncAppl "Just" [ElmExprIdent . textHeadToUpper $ i])
          . zip [1 :: Int ..]
          $ as
        )
        | Inst i as _ _ <- insts
      ] ++ [("_", "Nothing")]
  ]
  where readArg :: Type -> ElmExpr -> ElmExpr
        readArg (RegT _)  e = e
        readArg (BitsT _) e = e
        readArg (IntT w)  e =
          ElmBinOp
            ( ElmBinOp
              (ElmFuncAppl (ElmMember "Maybe" "map") [ElmExprIdent $ "int" ++ tshow w])
              "<<"
              (ElmMember "String" "toInt")
            )
            "<|"
            e
        readArg  InstT    e = e

genDecodeInst :: Proc -> ElmStmt
genDecodeInst Proc{..} = ElmStmts
  [ ElmTypeSig "decodeInst"
      ( ElmFuncType
          ( ElmTypeFuncAppl "IntW"
            [ ElmTypeIdent
              . ("Num" ++)
              $ ( tshow
                . fromMaybe 0
                . headMay
                . mapMaybe (\case
                    EncType InstT n -> Just n
                    _               -> Nothing
                  )
                $ encTypes
                )
            ]
          )
          "Inst"
        )
  , ElmDef (ElmPatFuncAppl "decodeInst" ["x"])
    . ElmCaseExpr (ElmFuncAppl "intToBits" ["x"])
    $ [ ( ElmListPat . genBitsPat . ConcatBitsExpr (length bs + sizeOfEnc enc) (ConstBitsExpr bs) $ enc
        , ElmFuncAppl (ElmExprIdent . textHeadToUpper $ i) (zipWith genArg ts as)
        )
        | Inst i ts _ (as, (bs, enc)) <- insts
      ] ++ [("_", "Halt")]
  ]
  where genBitsPat :: BitsExpr -> [ElmPattern]
        genBitsPat (ConstBitsExpr bs)       = map (\case Zero -> "False"; One -> "True") bs
        genBitsPat (EncBitsExpr n i)        = map (ElmPatIdent . (i ++) . tshow) [1..n]
        genBitsPat (ConcatBitsExpr _ e1 e2) = genBitsPat e1 ++ genBitsPat e2
        genArg :: Type -> Text -> ElmExpr
        genArg (RegT _)  _ = ElmStringExpr ""
        genArg (BitsT _) _ = ElmTupleExpr []
        genArg (IntT w)  i = ElmFuncAppl "bitsToInt" [ElmListExpr . map (ElmExprIdent . (i ++) . tshow) $ [1..w]]
        genArg  InstT    _ = ElmTupleExpr []

genEncodeInst :: Proc -> ElmStmt
genEncodeInst Proc{..} = ElmStmts
  [ ElmTypeSig "encodeInst"
      ( ElmFuncType
          "Inst"
          ( ElmTypeFuncAppl "IntW"
            [ ElmTypeIdent
              . ("Num" ++)
              $ ( tshow
                . fromMaybe 0
                . headMay
                . mapMaybe (\case
                    EncType InstT n -> Just n
                    _               -> Nothing
                  )
                $ encTypes
                )
            ]
          )
        )
  , ElmDef (ElmPatFuncAppl "encodeInst" ["i"])
    . ElmCaseExpr "i"
    $ [ ( ElmPatFuncAppl (textHeadToUpper i) (zipWith (\n _ -> ElmPatIdent . ("arg" ++) . tshow $ n) [1 :: Int ..] as)
        , encode ts as (ConcatBitsExpr (length bs + sizeOfEnc enc) (ConstBitsExpr bs) enc)
        )
        | Inst i ts _ (as, (bs, enc)) <- insts
      ]
  ]
  where encode :: [Type] -> [Text] -> BitsExpr -> ElmExpr
        encode _  _  (ConstBitsExpr bs) = ElmFuncAppl (ElmExprIdent . ("int" ++) . tshow . length $ bs) [ElmExprInt . bitsToInt Big $ bs]
        encode ts as (EncBitsExpr _ i) =
          case headMay . mapMaybe (\(n, (t, a)) -> if a == i then Just (n, t) else Nothing) . zip [1 :: Int ..] $ zip ts as of
            Just (_, RegT _)  -> error "Reg argument not supported"
            Just (_, BitsT _) -> error "Bits argument not supported"
            Just (n, IntT _)  -> ElmExprIdent $ "arg" ++ tshow n
            Just (_, InstT)   -> error "Inst argument not supported"
            Nothing           -> error "Argument not found"
        encode ts as (ConcatBitsExpr _ e1 (ConstBitsExpr [])) = encode ts as e1
        encode ts as (ConcatBitsExpr _ (ConstBitsExpr []) e2) = encode ts as e2
        encode ts as (ConcatBitsExpr _ e1 e2) = ElmFuncAppl (ElmExprIdent $ "concatBits" ++ (tshow . sizeOfEnc $ e2)) (map (encode ts as) [e2, e1])

genTick :: Proc -> ElmStmt
genTick Proc{..} = ElmStmts
  [ ElmTypeSig "tick"
    ( ElmFuncType
      "SimState"
      (ElmTypeFuncAppl "TickRes" ["SimState"])
    )
  , ElmDef (ElmPatFuncAppl "tick" ["simState"])
    . ElmLetIn "simState_"
        (ElmRecordUpdate "simState" (map (elmImplRule Proc{..}) always))
    . ElmLetIn "inst" (ElmFuncAppl "decodeInst" [ElmMember "simState" "inst"])
    . ElmRecord
    $ [ ( "debugMsg"
        , ElmFuncAppl "showInst" ["inst"]
        )
      , ( "newState"
        , ElmCaseExpr "inst"
          [ ( ElmPatFuncAppl (textHeadToUpper i) (map (ElmPatIdent . ("arg_" ++)) as)
            , ElmRecordUpdate "simState_" (map (elmImplRule Proc{..}) rs)
            )
            | Inst i _ (as, rs) _ <- insts
          ]
        )
      ]
  ]

genGetLeds :: Proc -> ElmStmt
genGetLeds Proc{..} = ElmStmts
  [ ElmTypeSig "getLeds"
    ( ElmFuncType
      "SimState"
      (ElmTypeFuncAppl "List" ["Bool"])
    )
  , ElmDef (ElmPatFuncAppl "getLeds" ["simState"]) (ledsFrom 0 . sortOn (\(LedImpl _ n _) -> n) $ leds)
  ]
  where ledsFrom :: Int -> [LedImpl] -> ElmExpr
        ledsFrom _ [] = ElmFuncAppl "int0" [0]
        ledsFrom n' ls@[LedImpl m n e]
          | n' == n   = ElmFuncAppl "intToBits" [elmExpr Proc{..} (m - n + 1) e]
          | otherwise = ElmBinOp (ElmFuncAppl (ElmMember "List" "repeat") [ElmExprInt (n - n'), "False"]) "++" (ledsFrom n ls)
        ledsFrom n' ls@(LedImpl m n e : ls')
          | n' == n   = ElmBinOp (ElmFuncAppl "intToBits" [elmExpr Proc{..} (m - n + 1) e]) "++" (ledsFrom (m + 1) ls')
          | otherwise = ElmBinOp (ElmFuncAppl (ElmMember "List" "repeat") [ElmExprInt (n - n'), "False"]) "++" (ledsFrom n ls)

genGetInspectibleMems :: Proc -> ElmStmt
genGetInspectibleMems Proc{..} = ElmStmts
  [ ElmTypeSig "getInspectibleMems"
      ( ElmFuncType
          "SimState"
          ( ElmTypeFuncAppl "List"
              [ElmTypeFuncAppl "InspectibleMem" ["SimState"]]
          )
        )
  , let
      instMemIndex =
        headMay
        . mapMaybe (\case
            ImplRule (RegLValue "inst") (MemAccessExpr _ mem ind) ->
              Just (mem, ind)
            _ -> Nothing
          )
        $ always
    in
    ElmDef (ElmPatFuncAppl "getInspectibleMems" ["simState"])
    . ElmListExpr
    $ [ ElmRecord
        [ ( "name"
          , ElmStringExpr i
          )
        , ( "contents"
          , ElmFuncAppl (ElmMember "List" "map")
            [ ElmLambda ["n"]
              . ElmRecord
              $ [ ( "value"
                  , ElmBinOp
                    ( ElmBinOp
                      ( ElmFuncAppl (ElmMember (ElmMember "Maybe" "Extra") "unwrap")
                        [ ElmStringExpr ""
                        , if (fst <$> instMemIndex) == Just i
                          then ElmBinOp "showInst" "<<" "decodeInst"
                          else ElmBinOp (ElmMember "String" "fromInt") "<<" "toInt"
                        ]
                      )
                      "<<"
                      (ElmFuncAppl (ElmMember "Array" "get") ["n"])
                    )
                    "<|"
                    (ElmMember "simState" i)
                  )
                , ( "set"
                  , ElmBinOp
                    ( ElmFuncAppl (ElmMember "Maybe" "map")
                      [ ElmLambda ["x"]
                        . ElmRecordUpdate "simState"
                        $ [ ( i
                            , ElmFuncAppl (ElmMember "Array" "set")
                              [ "n"
                              , ElmFuncAppl
                                ( if (fst <$> instMemIndex) == Just i
                                  then "encodeInst"
                                  else ElmExprIdent $ "int" ++ tshow dw
                                )
                                ["x"]
                              , ElmMember "simState" i
                              ]
                            )
                          ]
                      ]
                    )
                    "<<"
                    ( if (fst <$> instMemIndex) == Just i
                      then "readInst"
                      else ElmMember "String" "toInt"
                    )
                  )
                , ( "selected"
                  , maybe "False"
                    ( ElmBinOp
                        ( ElmFuncAppl
                          (ElmExprIdent $ "int" ++ tshow aw)
                          ["n"]
                        )
                        "=="
                      . elmExpr Proc{..} aw
                    )
                    ( instMemIndex
                      >>= \(instMem, instIndex) -> if instMem == i then Just instIndex else Nothing
                    )
                  )
                ]
            , ElmFuncAppl (ElmMember "List" "range")
                [ 0
                , ElmBinOp (ElmBinOp 2 "^" (ElmExprInt aw)) "-" 1
                ]
            ]
          )
        , ( "setAll"
          , ElmLambda ["xs"]
            . ElmRecordUpdate "simState"
            $ [ ( i
                , ElmBinOp
                  ( ElmBinOp
                    ( ElmFuncAppl
                      (ElmMember "Array" "indexedMap")
                      [ ElmLambda ["n"]
                        ( ElmBinOp
                          ( ElmFuncAppl
                            ( ElmMember
                              ( ElmMember
                                "Maybe"
                                "Extra"
                              )
                              "unwrap"
                            )
                            [ ElmFuncAppl "withDefault"
                              [ ElmFuncAppl
                                (ElmExprIdent $ "int" ++ tshow dw)
                                [0]
                              , ElmFuncAppl
                                (ElmMember "Array" "get")
                                [ "n"
                                , ElmMember "simState" i
                                ]
                              ]
                            , if (fst <$> instMemIndex) == Just i
                              then "encodeInst"
                              else ElmExprIdent $ "int" ++ tshow dw
                            ]
                          )
                          "<<"
                          ( if (fst <$> instMemIndex) == Just i
                            then "readInst"
                            else ElmMember "String" "toInt"
                          )
                        )
                      ]
                    )
                    "<<"
                    (ElmMember "Array" "fromList")
                  )
                  "<|"
                  "xs"
                )
              ]
          )
        ]
        | Memory i dw aw <- memorys
      ]
  ]

genSim :: Proc -> ElmStmt
genSim Proc{..} = ElmStmts
  [ ElmTypeSig "sim" (ElmTypeFuncAppl "Sim" ["SimState"])
  , ElmDef "sim"
    . ElmRecord
    $ [ ( "defaultState"
        , ElmRecord
          ( [ ( i
              , ElmFuncAppl (ElmExprIdent $ "int" ++ tshow w) [0]
              )
              | Reg i w _ <- regs 
            ] ++
            [ ( i
              , ElmFuncAppl (ElmMember "Array" "repeat")
                [ ElmBinOp 2 "^" (ElmExprInt aw)
                , ElmFuncAppl (ElmExprIdent $ "int" ++ tshow dw) [0]
                ]
              )
              | Memory i dw aw <- memorys
            ]
          )
        )
      , ( "performButton"
        , "performButton"
        )
      , ( "getButtonName"
        , "getButtonName"
        )
      , ( "runButton"
        , ElmExprInt
          . fromMaybe 0
          . headMay
          . mapMaybe (\case
              Button "run" n _ -> Just n
              _                -> Nothing
            )
          $ buttons
        )
      , ( "executeButton"
        , ElmExprInt
          . fromMaybe 0
          . headMay
          . mapMaybe (\case
              Button "execute" n _ -> Just n
              _                    -> Nothing
            )
          $ buttons
        )
      , ( "tick"
        , "tick"
        )
      , ( "getLeds"
        , "getLeds"
        )
      , ( "getInspectibleMems"
        , "getInspectibleMems"
        )
      ]
  ]

genMain :: Proc -> ElmStmt
genMain _ = ElmStmts
  [ ElmTypeSig "main" (ElmTypeFuncAppl "Interface" ["SimState"])
  , ElmDef "main" (ElmFuncAppl "interface" ["sim"])
  ]

genElm :: Proc -> ElmStmt
genElm = ElmStmts . intersperse (ElmStmts . replicate 2 $ ElmBlankLine) . flap
  [ genHeader
  , genSimState
  , genPerformButton
  , genGetButtonName
  , genInst
  , genShowInst
  , genReadInst
  , genDecodeInst
  , genEncodeInst
  , genTick
  , genGetLeds
  , genGetInspectibleMems
  , genSim
  , genMain
  ]

writeElm :: Handle -> Proc -> IO ()
writeElm h = hPutStrLn h . pretty . genElm

genSimulator :: FilePath -> Proc -> IO ()
genSimulator fn ast =
  withSystemTempDirectory (takeBaseName fn) $ \dir -> do
    jsonSrc <- getDataFileName "simulator/elm.json"
    copyFile jsonSrc (dir </> "elm.json")
    createDirectory (dir </> "src")
    mapM_ (\f -> getDataFileName ("simulator/src/" ++ f) >>= flip copyFile (dir </> "src" </> f))
      [ "Interface.elm"
      , "IntWidths.elm"
      , "Style.elm"
      ]
    withFile (dir </> "src/Main.elm") WriteMode $ \h ->
      writeElm h ast
    fnAbs <- (</> fn) <$> getCurrentDirectory
    (_, _, _, ph) <- createProcess $ (proc "elm" ["make", "--output=" ++ fnAbs, "src/Main.elm"]) { cwd = Just dir }
    void . waitForProcess $ ph

