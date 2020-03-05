{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

{-|
Module      : GenSimulator
Description : Generate a simulator
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module GenSimulator
  ( genElm
  , genSimulatorBS
  ) where

import ClassyPrelude

import Data.Bit (Bit(..))
import Data.Bit.List (Endianness(..), bitsToInt)
import Data.List (foldl)
import Data.List.Group (groupWith)
import Data.Maybe (fromMaybe, mapMaybe)
import Language.Elm.AST
import Language.Elm.Pretty (pretty)
import Language.Foundry.Proc
import Maps.Text (textHeadToUpper)
import Paths_foundry
import System.Directory (copyFile, createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.Process (callProcess)

elmType :: Type -> ElmType
elmType (RegT n)  = ElmTypeIdent $ "Reg" ++ tshow n
elmType (BitsT _) = ElmTupleType []
elmType (IntT n)  = ElmTypeFuncAppl "IntW" [ElmTypeIdent $ "Num" ++ tshow n]
elmType  InstT    = ElmTupleType []

elmOp :: Op -> Int -> ElmExpr
elmOp Add        _ = ElmFuncAppl "binOpW" [ElmParenExpr "+"]
elmOp Sub        _ = ElmFuncAppl "binOpW" [ElmParenExpr "-"]
elmOp Mul        _ = ElmFuncAppl "binOpW" [ElmParenExpr "*"]
elmOp Div        _ = ElmFuncAppl "binOpW" [ElmParenExpr "/"]
elmOp ConcatBits w = ElmExprIdent $ "concatBits" ++ tshow w
elmOp BitwiseAnd _ = ElmFuncAppl "binOpW" [ElmMember "Bitwise" "and"]
elmOp BitwiseOr  _ = ElmFuncAppl "binOpW" [ElmMember "Bitwise" "or"]
elmOp BitwiseXor _ = ElmFuncAppl "binOpW" [ElmMember "Bitwise" "xor"]

elmBoolExpr :: Proc -> [Type] -> [Text] -> BoolExpr -> ElmExpr
elmBoolExpr Proc{..} ts as (EqualityExpr e1 e2)   = ElmBinOp (elmExpr Proc{..} ts as 1 e1) "==" (elmExpr Proc{..} ts as 1 e2)
elmBoolExpr Proc{..} ts as (InequalityExpr e1 e2) = ElmBinOp (elmExpr Proc{..} ts as 1 e1) "/=" (elmExpr Proc{..} ts as 1 e2)
elmBoolExpr Proc{..} ts as (LogicalAndExpr e1 e2) = ElmBinOp (elmBoolExpr Proc{..} ts as e1) "&&" (elmBoolExpr Proc{..} ts as e2)
elmBoolExpr Proc{..} ts as (LogicalOrExpr e1 e2)  = ElmBinOp (elmBoolExpr Proc{..} ts as e1) "||" (elmBoolExpr Proc{..} ts as e2)

elmExpr :: Proc -> [Type] -> [Text] -> Int -> Expr -> ElmExpr
elmExpr Proc{..} ts as _ (VarExpr _ i)         =
  case lookup i $ zip as ts of
    Just (RegT w) ->
      ElmFuncAppl (ElmExprIdent $ "getReg" ++ tshow w)
        [ ElmExprIdent $ "arg_" ++ i
        , "simState"
        ]
    _ -> ElmExprIdent $ "arg_" ++ i
elmExpr Proc{..} _  _  _ (RegExpr _ i)         = ElmMember "simState" i
elmExpr Proc{..} ts as _ (MemAccessExpr _ i e) =
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
        [ ElmFuncAppl "toInt" [elmExpr Proc{..} ts as aw e]
        , ElmMember "simState" i
        ]
    ]
elmExpr Proc{..} _  _  w (ConstExpr n)         = ElmFuncAppl (ElmExprIdent $ "int" ++ tshow w) [ElmExprInt n]
elmExpr Proc{..} _  _  _ (BinaryConstExpr bs)  = ElmFuncAppl (ElmExprIdent $ "int" ++ (tshow . length $ bs)) [ElmExprInt . bitsToInt Big $ bs]
elmExpr Proc{..} ts as w (OpExpr _ o e1 e2)    = ElmFuncAppl (elmOp o (fromMaybe 0 . widthOfExpr $ e1)) [elmExpr Proc{..} ts as w e1, elmExpr Proc{..} ts as w e2]
elmExpr Proc{..} ts as w (TernaryExpr _ c t f) = ElmTernOp (elmBoolExpr Proc{..} ts as c) (elmExpr Proc{..} ts as w t) (elmExpr Proc{..} ts as w f)

elmImplRule :: Proc -> [Type] -> [Text] -> ImplRule -> ElmExpr
elmImplRule Proc{..} ts as (ImplRule (VarLValue i) e) =
  let w = maybe 0 (\(RegT w') -> w') . lookup i $ zip as ts in
  ElmFuncAppl
    (ElmExprIdent $ "setReg" ++ tshow w)
    [ ElmExprIdent $ "arg_" ++ i
    , elmExpr Proc{..} ts as w e
    ]
elmImplRule Proc{..} ts as (ImplRule (RegLValue i) e) =
  let w =
        fromMaybe 0
        . headMay
        . mapMaybe (\(Reg i' w' _) -> if i == i' then Just w' else Nothing)
        $ regs
  in
  ElmFuncAppl
    (ElmExprIdent $ "setReg" ++ tshow w)
    [ ElmExprIdent . textHeadToUpper $ i
    , elmExpr Proc{..} ts as w e
    ]
elmImplRule Proc{..} ts as (ImplRule (MemAccessLValue i e1) e2) =
  let w =
        fromMaybe 0
        . headMay
        . mapMaybe (\(Memory i' dw _) -> if i == i' then Just dw else Nothing)
        $ memorys
  in
  ElmLambda ["s"]
    ( ElmRecordUpdate "s"
      [ ( i
        , ElmFuncAppl (ElmMember "Array" "set")
          [ ElmFuncAppl "toInt" [elmExpr Proc{..} ts as w e1]
          , elmExpr Proc{..} ts as w e2
          , ElmMember "simState" i
          ]
        )
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
        , ElmFuncAppl (ElmMember "List" "foldr")
          [ ElmParenExpr "<|"
          , "simState"
          , ElmListExpr
            [ elmImplRule Proc{..} [] [] r
            | r <- rs
            ]
          ]
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

genRegs :: Proc -> ElmStmt
genRegs Proc{..} = ElmStmts . intersperse (ElmStmts . replicate 2 $ ElmBlankLine) $
  [ ElmStmts
    [ ElmTypeDef ("Reg" ++ tshow size)
      [ ( textHeadToUpper i
        , []
        )
      | Reg i _ _ <- rs
      ]
    , ElmBlankLine
    , ElmBlankLine
    , ElmTypeSig ("getReg" ++ tshow size)
      ( ElmFuncType (ElmTypeIdent $ "Reg" ++ tshow size)
      . ElmFuncType "SimState"
      $ (elmType . IntT $ size)
      )
    , ElmDef (ElmPatFuncAppl ("getReg" ++ tshow size) ["r"])
      . ElmCaseExpr "r"
      $ [ ( ElmPatIdent . textHeadToUpper $ i
          , ElmParenExpr . ElmExprIdent $ "." ++ i
          )
        | Reg i _ _ <- rs
        ]
    , ElmBlankLine
    , ElmBlankLine
    , ElmTypeSig ("setReg" ++ tshow size)
      ( ElmFuncType (ElmTypeIdent $ "Reg" ++ tshow size)
      . ElmFuncType (elmType . IntT $ size)
      . ElmFuncType "SimState"
      $ "SimState"
      )
    , ElmDef (ElmPatFuncAppl ("setReg" ++ tshow size) ["r", "x", "s"])
      . ElmCaseExpr "r"
      $ [ ( ElmPatIdent . textHeadToUpper $ i
          , ElmRecordUpdate "s" [(i, "x")]
          )
        | Reg i _ _ <- rs
        ]
    ]
  | (size, rs) <- groupWith (\(Reg _ n _) -> n) regs
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
        showArg (RegT n)  e = ElmFuncAppl (ElmExprIdent $ "showReg" ++ tshow n) [e]
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
        readArg (RegT w)  e = ElmFuncAppl (ElmExprIdent $ "readReg" ++ tshow w) [e]
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

genShowRegs :: Proc -> ElmStmt
genShowRegs Proc{..} = ElmStmts
  [ ElmStmts
    [ ElmTypeSig ("showReg" ++ tshow size)
        ( ElmFuncType
            (ElmTypeIdent $ "Reg" ++ tshow size)
            "String"
        )
    , ElmDef (ElmPatFuncAppl ("showReg" ++ tshow size) ["s"])
      . ElmCaseExpr "s"
      $ [ ( ElmPatIdent . textHeadToUpper $ i
          , ElmStringExpr i
          )
        | Reg i _ _ <- rs
        ]
    ]
  | (size, rs) <- groupWith (\(Reg _ n _) -> n) regs
  ]

genReadRegs :: Proc -> ElmStmt
genReadRegs Proc{..} = ElmStmts
  [ ElmStmts
    [ ElmTypeSig ("readReg" ++ tshow size)
        ( ElmFuncType
            "String"
            (ElmTypeFuncAppl "Maybe" [ElmTypeIdent $ "Reg" ++ tshow size])
        )
    , ElmDef (ElmPatFuncAppl ("readReg" ++ tshow size) ["s"])
      . ElmCaseExpr "s"
      $ [ ( ElmStringPat i
          , ElmFuncAppl "Just" [ElmExprIdent . textHeadToUpper $ i]
          )
        | Reg i _ _ <- rs
        ] ++ [("_", "Nothing")]
    ]
  | (size, rs) <- groupWith (\(Reg _ n _) -> n) regs
  ]

decodeBits :: BitsExpr -> [ElmPattern]
decodeBits (ConstBitsExpr bs)       = map (\case Zero -> "False"; One -> "True") bs
decodeBits (EncBitsExpr n i)        = map (ElmPatIdent . (i ++) . tshow) [1..n]
decodeBits (ConcatBitsExpr _ e1 e2) = decodeBits e1 ++ decodeBits e2

encodeBits :: [Type] -> [Text] -> BitsExpr -> ElmExpr
encodeBits _  _  (ConstBitsExpr bs) = ElmFuncAppl "bitsToInt" ["Little", ElmListExpr . map (\case Zero -> "False"; One -> "True") $ bs]
encodeBits ts as (EncBitsExpr _ i) =
  case headMay . mapMaybe (\(n, (t, a)) -> if a == i then Just (n, t) else Nothing) . zip [1 :: Int ..] $ zip ts as of
    Just (n, RegT w)  -> ElmFuncAppl (ElmExprIdent $ "encodeReg" ++ tshow w) [ElmExprIdent $ "arg" ++ tshow n]
    Just (_, BitsT _) -> error "Bits argument not supported"
    Just (n, IntT _)  -> ElmExprIdent $ "arg" ++ tshow n
    Just (_, InstT)   -> error "Inst argument not supported"
    Nothing           -> error "Argument not found"
encodeBits ts as (ConcatBitsExpr _ e1 (ConstBitsExpr [])) = encodeBits ts as e1
encodeBits ts as (ConcatBitsExpr _ (ConstBitsExpr []) e2) = encodeBits ts as e2
encodeBits ts as (ConcatBitsExpr _ e1 e2) = ElmFuncAppl (ElmExprIdent $ "concatBits" ++ (tshow . sizeOfEnc $ e1)) (map (encodeBits ts as) [e1, e2])

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
    . ElmCaseExpr (ElmFuncAppl "intToBits" ["Little", "x"])
    $ [ ( ElmListPat . decodeBits . ConcatBitsExpr (length bs + sizeOfEnc enc) (ConstBitsExpr bs) $ enc
        , ElmFuncAppl (ElmExprIdent . textHeadToUpper $ i) (zipWith genArg ts as)
        )
      | Inst i ts _ (as, (bs, enc)) <- insts
      ] ++ [("_", "Halt")]
  ]
  where genArg :: Type -> Text -> ElmExpr
        genArg (RegT w)  i = ElmFuncAppl (ElmExprIdent $ "decodeReg" ++ tshow w)
                               [ ElmListExpr
                                 . map ( ElmExprIdent . (i ++) . tshow)
                                 $ [  1 
                                   .. ( fromMaybe 0
                                      . headMay
                                      . mapMaybe (\case
                                          EncType (RegT w') n
                                            | w' == w   -> Just n
                                            | otherwise -> Nothing
                                          _             -> Nothing
                                        )
                                      $ encTypes
                                      )
                                   ]
                               ]
        genArg (BitsT _) _ = ElmTupleExpr []
        genArg (IntT w)  i = ElmFuncAppl "bitsToInt"
                               [ "Little"
                               , ElmListExpr
                                 . map ( ElmExprIdent . (i ++) . tshow)
                                 $ [  1 
                                   .. w
                                   ]
                               ]
        genArg  InstT    _ = ElmTupleExpr []

genEncodeInst :: Proc -> ElmStmt
genEncodeInst Proc{..} = ElmStmts
  [ ElmTypeSig "encodeInst"
    ( ElmFuncType
      "Inst"
      ( ElmTypeFuncAppl "IntW"
        [ ElmTypeIdent
          . ("Num" ++)
          . tshow
          . fromMaybe 0
          . headMay
          . mapMaybe (\case
              EncType InstT n -> Just n
              _               -> Nothing
            )
          $ encTypes
        ]
      )
    )
  , ElmDef (ElmPatFuncAppl "encodeInst" ["i"])
    . ElmCaseExpr "i"
    $ [ ( ElmPatFuncAppl (textHeadToUpper i) (zipWith (\n _ -> ElmPatIdent . ("arg" ++) . tshow $ n) [1 :: Int ..] as)
        , encodeBits ts as (ConcatBitsExpr (length bs + sizeOfEnc enc) (ConstBitsExpr bs) enc)
        )
      | Inst i ts _ (as, (bs, enc)) <- insts
      ]
  ]

genDecodeRegs :: Proc -> ElmStmt
genDecodeRegs Proc{..} = ElmStmts . intersperse (ElmStmts . replicate 2 $ ElmBlankLine) $
  [ ElmStmts
    [ ElmTypeSig
      ("decodeReg" ++ tshow size)
      ( ElmFuncType
        (ElmTypeFuncAppl "List" ["Bool"])
        ( ElmTypeIdent
        . ("Reg" ++)
        . tshow
        $ size
        )
      )
    , ElmDef (ElmPatFuncAppl ("decodeReg" ++ tshow size) ["r"])
      . ElmCaseExpr "r"
      $ [ ( ElmListPat . decodeBits . ConstBitsExpr $ enc
          , ElmExprIdent . textHeadToUpper $ i
          )
        | (Reg i _ (Just enc)) <- rs
        ] ++ [("_", (\(Reg i _ _ : _) -> ElmExprIdent . textHeadToUpper $ i) regs)]
    ]
  | (size, rs) <- groupWith (\(Reg _ n _) -> n) . filter (\(Reg _ _ e) -> not . null $ e) $ regs
  ]

genEncodeRegs :: Proc -> ElmStmt
genEncodeRegs Proc{..} = ElmStmts . intersperse (ElmStmts . replicate 2 $ ElmBlankLine) $
  [ ElmStmts
    [ ElmTypeSig
      ("encodeReg" ++ tshow size)
      ( ElmFuncType
        ( ElmTypeIdent
        . ("Reg" ++)
        . tshow
        $ size
        )
        ( ElmTypeFuncAppl "IntW"
          [ ElmTypeIdent
            . ("Num" ++)
            . tshow
            . fromMaybe 0
            . headMay
            . mapMaybe (\case
                EncType (RegT w) n
                  | w == size -> Just n
                  | otherwise -> Nothing
                _             -> Nothing
              )
            $ encTypes
          ]
        )
      )
    , ElmDef (ElmPatFuncAppl ("encodeReg" ++ tshow size) ["r"])
      . ElmCaseExpr "r"
      $ [ ( ElmPatIdent . textHeadToUpper $ i
          , encodeBits [] [] (ConstBitsExpr enc)
          )
        | (Reg i _ (Just enc)) <- rs
        ] ++ [("_", encodeBits [] [] (ConstBitsExpr []))]
    ]
  | (size, rs) <- groupWith (\(Reg _ n _) -> n) . filter (\(Reg _ _ e) -> not . null $ e) $ regs
  ]

genTick :: Proc -> ElmStmt
genTick Proc{..} = ElmStmts
  [ ElmTypeSig "tick"
    ( ElmFuncType
      "SimState"
      (ElmTypeFuncAppl "TickRes" ["SimState"])
    )
  , ElmDef (ElmPatFuncAppl "tick" ["simState"])
    . ElmLetIn "inst" (ElmFuncAppl "decodeInst" [ElmMember "simState" "inst"])
    . ElmRecord
    $ [ ( "debugMsg"
        , ElmFuncAppl "showInst" ["inst"]
        )
      , ( "newState"
        , ElmCaseExpr "inst"
          [ ( ElmPatFuncAppl (textHeadToUpper i) (map (ElmPatIdent . ("arg_" ++)) as)
            , ElmFuncAppl (ElmMember "List" "foldr")
              [ ElmParenExpr "<|"
              , "simState"
              , ElmListExpr
                [ elmImplRule Proc{..} ts as r
                | r <- rs ++ always
                ]
              ]
            )
          | Inst i ts (as, rs) _ <- insts
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
          | n' == n   = ElmFuncAppl "intToBits" ["Little", elmExpr Proc{..} [] [] (m - n + 1) e]
          | otherwise = ElmBinOp (ElmFuncAppl (ElmMember "List" "repeat") [ElmExprInt (n - n'), "False"]) "++" (ledsFrom n ls)
        ledsFrom n' ls@(LedImpl m n e : ls')
          | n' == n   = ElmBinOp (ElmFuncAppl "intToBits" ["Little", elmExpr Proc{..} [] [] (m - n + 1) e]) "++" (ledsFrom (m + 1) ls')
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
                , ( "getHex"
                  , ElmTupleExpr
                    [ ElmExprInt dw
                    , ElmBinOp
                      ( ElmBinOp
                        (ElmFuncAppl (ElmMember (ElmMember "Maybe" "Extra") "unwrap") [0, "toInt"])
                        "<<"
                        (ElmFuncAppl (ElmMember "Array" "get") ["n"])
                      )
                      "<|"
                      (ElmMember "simState" i)
                    ]
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
                      . elmExpr Proc{..} [] [] aw
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
                    (ElmMember "Array" "fromList")
                    "<<"
                    ( ElmFuncAppl
                      (ElmMember "List" "indexedMap")
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
                  )
                  "<|"
                  "xs"
                )
              ]
          )
        , ( "setAllHex"
          , ElmLambda ["xs"]
            . ElmRecordUpdate "simState"
            $ [ ( i
                , ElmBinOp
                  ( ElmBinOp
                    (ElmMember "Array" "fromList")
                    "<<"
                    (ElmFuncAppl (ElmMember "List" "map") [ElmExprIdent $ "int" ++ tshow dw])
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

genGetRegValues :: Proc -> ElmStmt
genGetRegValues Proc{..} = ElmStmts
  [ ElmTypeSig "getRegValues"
      ( ElmFuncType
          "SimState"
          ( ElmTypeFuncAppl "List"
              [ElmTupleType ["String", "Int"]]
          )
        )
  , ElmDef (ElmPatFuncAppl "getRegValues" ["simState"])
    . ElmListExpr
    $ [ ElmTupleExpr [ElmStringExpr i, ElmFuncAppl "toInt" [ElmMember "simState" i]]
      | Reg i _ _ <- regs
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
      , ( "getRegValues"
        , "getRegValues"
        )
      ]
  ]

genMain :: Proc -> ElmStmt
genMain _ = ElmStmts
  [ ElmTypeSig "main" (ElmTypeFuncAppl "Interface" ["SimState"])
  , ElmDef "main" (ElmFuncAppl "interface" ["sim"])
  ]

flap :: Functor f => f (a -> b) -> a -> f b
flap fs = flip fmap fs . flip ($)

genElm :: Proc -> ElmStmt
genElm = ElmStmts . intersperse (ElmStmts . replicate 2 $ ElmBlankLine) . flap
  [ genHeader
  , genSimState
  , genPerformButton
  , genGetButtonName
  , genRegs
  , genInst
  , genShowInst
  , genReadInst
  , genShowRegs
  , genReadRegs
  , genDecodeInst
  , genEncodeInst
  , genDecodeRegs
  , genEncodeRegs
  , genTick
  , genGetLeds
  , genGetInspectibleMems
  , genGetRegValues
  , genSim
  , genMain
  ]

genErrorPage :: Text -> ElmStmt
genErrorPage e = ElmStmts
  [ ElmModule "Main" ["main"]
  , ElmBlankLine
  , ElmImport "Interface" [".."]
  , ElmBlankLine
  , ElmTypeSig "main" (ElmTypeFuncAppl "Interface" [ElmTupleType []])
  , ElmDef "main" (ElmFuncAppl "errorPage" [ElmStringExpr e])
  ]

copyFileMakingParents :: FilePath -> FilePath -> IO ()
copyFileMakingParents src dest = do
  createDirectoryIfMissing True (takeDirectory dest)
  copyFile src dest

runElm :: ElmStmt -> IO ByteString
runElm elm = do
  dir <- (</> "foundry-simulator") <$> getCanonicalTemporaryDirectory
  let elmDir = dir </> "elm"
  let elmSrcDir = elmDir </> "src"
  elmJsonSrc <- getDataFileName "simulator/elm/elm.json"
  copyFileMakingParents elmJsonSrc (elmDir </> "elm.json")
  mapM_ (\f -> getDataFileName ("simulator/elm/src/" ++ f) >>= flip copyFileMakingParents (elmSrcDir </> f))
    [ "Burn.elm"
    , "Bootstrap/Form/Range.elm"
    , "Hex.elm"
    , "Icons.elm"
    , "Interface.elm"
    , "IntWidths.elm"
    , "List/Pad.elm"
    , "WebUSB.elm"
    ]
  writeFile (elmSrcDir </> "Main.elm") . encodeUtf8 . pretty $ elm
  bs <- withCurrentDirectory elmDir $ do
    callProcess "elm" ["make", "--optimize", "--output=main.js", "src/Main.elm"]
    readFile "main.js"
  return ("function Main() {\n" ++ bs ++ "\n}\nexport const Elm = new Main().Elm;\n")

genSimulatorBS :: MonadIO m => Either Text Proc -> m ByteString
genSimulatorBS = liftIO . runElm . either genErrorPage genElm

