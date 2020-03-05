{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards, TupleSections #-}

{-|
Module      : GenVerilog
Description : Generate a Verilog AST for a processor
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module GenVerilog
  ( genVerilog
  ) where

import ClassyPrelude

import Data.Bit (Bit)
import Data.Bit.List (Endianness(..), bitsToInt)
import Data.List.Group (groupWith)
import Language.Foundry.Proc
import Language.Verilog.Align
import qualified Language.Verilog.AST as V
import Language.Verilog.Optimiser (optimise)
import Language.Verilog.Output (output)

import Data.Maybe (mapMaybe, maybeToList)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set (fromList, toList)

combineBlocks :: [a -> V.Verilog] -> a -> V.Verilog
combineBlocks fs = V.Seq . flip fmap fs . flip ($)

genBits :: [Bit] -> Text
genBits bs = (tshow . length $ bs) ++ "'b" ++ (concatMap tshow . reverse $ bs)

genInstDef :: Inst -> V.Verilog
genInstDef (Inst n _ _ (_, (bs, _))) = V.Define ("is_inst_" ++ n) ["inst"]
  ("(inst[" ++ tshow (length bs - 1) ++ ":0] == " ++ genBits bs ++ ")")

genInstDefs :: Proc -> V.Verilog
genInstDefs = V.Seq . map genInstDef . insts

genButtonDef :: Button -> V.Verilog
genButtonDef (Button n i _) = V.Define ("BUTTON_" ++ n) [] ("buttons[" ++ tshow i ++ "]")

genButtonDefs :: Proc -> V.Verilog
genButtonDefs = V.Seq . map genButtonDef . buttons

genDefs :: Proc -> V.Verilog
genDefs = combineBlocks [genInstDefs, genButtonDefs]

includes :: Proc -> V.Verilog
includes = const . V.Seq $
  [ V.Include "ram.v"
  , V.Include "prescaler.v"
  , V.Include "single_trigger.v"
  ]

genPreamble :: Proc -> V.Verilog
genPreamble = combineBlocks [includes, genDefs]

boilerplateRegs :: Proc -> V.Verilog
boilerplateRegs = const . V.Seq . map (V.Seq . map V.RawVerilog) $
  [ [ "  // Handle running"
    , "  reg running = 0;"
    , "  always @ (posedge `BUTTON_run) begin"
    , "    running <= !running;"
    , "  end"
    ]
  , [ "  // Generate running clock"
    , "  wire running_counter;"
    , "  PRESCALER #(.BITS(14)) scal0 (.clk(clk), .out(running_counter));"
    , "  wire running_clk = running & running_counter;"
    ]
  , [ "  // Handle execution"
    , "  wire running_trigger;"
    , "  SINGLE_TRIGGER trig1 (.clk(clk), .trigger_in(running_clk), .trigger_out(running_trigger));"
    , "  wire execute = (!running & execute_trigger) | running_trigger;"
    ]
  , [ "  // Handle halt"
    , "  reg halt = 0;"
    , "  always @ (posedge clk) begin"
    , "    if (execute & `is_inst_halt(inst))"
    , "      halt <= 1;"
    , "    if (!running)"
    , "      halt <= 0;"
    , "  end "
    ]
  ]

genButtonWire :: Button -> V.Verilog
genButtonWire (Button name _ _) = V.Wire 1 (name ++ "_trigger") Nothing Nothing

genButtonTrigger :: Button -> V.Verilog
genButtonTrigger (Button name _ _) =
  V.RawVerilog $ "  SINGLE_TRIGGER trig_" ++ name ++ " (.clk(clk), .trigger_in(`BUTTON_" ++ name ++ "), .trigger_out(" ++ name ++ "_trigger));\n"

genButtonTriggers :: Proc -> V.Verilog
genButtonTriggers = combineBlocks
  [ V.Seq . map genButtonWire . buttons
  , V.Seq . map genButtonTrigger . buttons
  ]

genOp :: Op -> Text
genOp Add        = "+"
genOp Sub        = "-"
genOp Mul        = "*"
genOp Div        = "/"
genOp ConcatBits = "++"
genOp BitwiseAnd = "&"
genOp BitwiseOr  = "|"
genOp BitwiseXor = "^"

genBoolExpr :: [Type] -> [Text] -> ([Text], ([Bit], BitsExpr)) -> BoolExpr -> V.Expr
genBoolExpr argTypes ruleArgs (encArgs, (bits, enc)) (EqualityExpr e1 e2)   = V.BinaryOp
  (genExpr argTypes ruleArgs (encArgs, (bits, enc)) e1)
  "=="
  (genExpr argTypes ruleArgs (encArgs, (bits, enc)) e2)
genBoolExpr argTypes ruleArgs (encArgs, (bits, enc)) (InequalityExpr e1 e2) = V.BinaryOp
  (genExpr argTypes ruleArgs (encArgs, (bits, enc)) e1)
  "!="
  (genExpr argTypes ruleArgs (encArgs, (bits, enc)) e2)
genBoolExpr argTypes ruleArgs (encArgs, (bits, enc)) (LogicalAndExpr b1 b2) = V.BinaryOp
  (genBoolExpr argTypes ruleArgs (encArgs, (bits, enc)) b1)
  "&&"
  (genBoolExpr argTypes ruleArgs (encArgs, (bits, enc)) b2)
genBoolExpr argTypes ruleArgs (encArgs, (bits, enc)) (LogicalOrExpr b1 b2)  = V.BinaryOp
  (genBoolExpr argTypes ruleArgs (encArgs, (bits, enc)) b1)
  "||"
  (genBoolExpr argTypes ruleArgs (encArgs, (bits, enc)) b2)

genExpr :: [Type] -> [Text] -> ([Text], ([Bit], BitsExpr)) -> Expr -> V.Expr
genExpr argTypes ruleArgs (encArgs, (bits, enc)) e = case e of
  VarExpr _ var         ->
    case lookup var $ zip ruleArgs (zip argTypes encArgs) of
      Just (argType, encArg) ->
        case argType of
          RegT size ->
            case findVarInEnc encArg (length bits) enc of
              Just (i, j) ->
                V.Index
                  (V.Variable $ "_regs" ++ tshow size)
                  (V.Index "inst" (V.Literal j) (V.Literal i))
                  (V.Index "inst" (V.Literal j) (V.Literal i))
              Nothing     -> error $ "Variable " ++ unpack var ++ " not used in encoding"
          BitsT _ ->
            case findVarInEnc encArg (length bits) enc of
              Just (i, j) -> V.Index "inst" (V.Literal j) (V.Literal i)
              Nothing     -> error $ "Variable " ++ unpack var ++ " not used in encoding"
          IntT _ ->
            case findVarInEnc encArg (length bits) enc of
              Just (i, j) -> V.Index "inst" (V.Literal j) (V.Literal i)
              Nothing     -> error $ "Variable " ++ unpack var ++ " not used in encoding"
          InstT -> error "Instruction argument"
      Nothing     -> error $ "No variable " ++ unpack var
  RegExpr _ reg         -> V.Variable reg
  MemAccessExpr _ mem _ -> V.Variable $ mem ++ "_out"
  ConstExpr n           -> V.Literal n
  BinaryConstExpr bs    -> V.Bits bs
  OpExpr _ o e1 e2      -> V.BinaryOp
    (genExpr argTypes ruleArgs (encArgs, (bits, enc)) e1)
    (genOp o)
    (genExpr argTypes ruleArgs (encArgs, (bits, enc)) e2)
  TernaryExpr _ b e1 e2 -> V.TernaryOp
    (genBoolExpr argTypes ruleArgs (encArgs, (bits, enc)) b)
    (genExpr argTypes ruleArgs (encArgs, (bits, enc)) e1)
    (genExpr argTypes ruleArgs (encArgs, (bits, enc)) e2)

genExpr' :: Expr -> V.Expr
genExpr' = genExpr [] [] ([], ([], ConstBitsExpr []))

genInstRule :: (ImplRule -> Maybe Expr) -> Inst -> Maybe (V.Expr, V.Expr)
genInstRule p (Inst name argTypes (ruleArgs, rules) (encArgs, (bits, enc))) =
  case mapMaybe p rules of
    []      -> Nothing
    [expr]  -> Just
      ( V.BinaryOp "execute" "&" (V.Variable $ "`is_inst_" ++ name ++ "(inst)")
      , genExpr argTypes ruleArgs (encArgs, (bits, enc)) expr
      )
    (_:_:_) -> error "More than one rule"

genIsInstRule :: (ImplRule -> Maybe Expr) -> Inst -> Maybe V.Expr
genIsInstRule p (Inst name _ (_, rules) (_, (_, _))) =
  case mapMaybe p rules of
    []      -> Nothing
    [_]     -> Just $ V.BinaryOp "execute" "&" (V.Variable $ "`is_inst_" ++ name ++ "(inst)")
    (_:_:_) -> error "More than one rule"

genButtonRule :: (ImplRule -> Maybe Expr) -> Button -> Maybe (V.Expr, V.Expr)
genButtonRule p (Button name _ rules) =
  case mapMaybe p rules of
    []      -> Nothing
    [expr]  -> Just
      ( V.BinaryOp (V.UnaryOp "!" "running") "&" (V.Variable $ name ++ "_trigger")
      , genExpr' expr
      )
    (_:_:_) -> error "More than one rule"

genIsButtonRule :: (ImplRule -> Maybe Expr) -> Button -> Maybe V.Expr
genIsButtonRule p (Button name _ rules) =
  case mapMaybe p rules of
    []      -> Nothing
    [_]     -> Just $ V.BinaryOp (V.UnaryOp "!" "running") "&" (V.Variable $ name ++ "_trigger")
    (_:_:_) -> error "More than one rule"

genAlwaysRule :: (ImplRule -> Maybe Expr) -> [ImplRule] -> Maybe (V.Expr, V.Expr)
genAlwaysRule p rules =
  case mapMaybe p rules of
    []      -> Nothing
    [expr]  -> Just
      ( "execute"
      , genExpr' expr
      )
    (_:_:_) -> error "More than one rule"

genIsAlwaysRule :: (ImplRule -> Maybe Expr) -> [ImplRule] -> Maybe V.Expr
genIsAlwaysRule p rules =
  case mapMaybe p rules of
    []      -> Nothing
    [_]     -> Just . V.Variable $ "execute"
    (_:_:_) -> error "More than one rule"

genNonEncRegDecl :: Reg -> V.Verilog
genNonEncRegDecl (Reg name size _) = V.Reg size name Nothing Nothing

genNonEncRegDecls :: Proc -> V.Verilog
genNonEncRegDecls = V.Seq . map genNonEncRegDecl . filter (\(Reg _ _ e) -> null e) . regs

genEncRegDecl :: (Int, [Reg]) -> V.Verilog
genEncRegDecl (size, rs) = V.Seq
  [ V.Reg size ("_regs" ++ tshow size) (Just $ n + 1) Nothing --(Just . V.RawExpr $ "{" ++ intercalate "0" (replicate n "0" :: [Text]) ++ "}")
  , V.Seq [ V.Wire size name Nothing (Just . V.RawExpr $ "_regs" ++ tshow size ++ "[" ++ genBits bs ++ "]") | Reg name _ (Just bs) <- rs]
  ]
  where n = maximum . ncons 0 . map (\(Reg _ _ (Just bs)) -> bitsToInt Little bs) $ rs

genEncRegDecls :: Proc -> V.Verilog
genEncRegDecls Proc{..} = V.Seq . map genEncRegDecl $ encRegs
  where encRegs = groupWith (\(Reg _ n _) -> n) . filter (\(Reg _ _ e) -> not . null $ e) $ regs

genRegDecls :: Proc -> V.Verilog
genRegDecls = combineBlocks [genNonEncRegDecls, genEncRegDecls]

zipCons :: [a] -> [[a]] -> [[a]]
zipCons  []     yss     = yss
zipCons  xs     []      = map (:[]) xs
zipCons (x:xs) (ys:yss) = (x:ys) : zipCons xs yss

transposeRagged :: [[a]] -> [[a]]
transposeRagged  []      = []
transposeRagged (xs:xss) = zipCons xs . transposeRagged $ xss

zipFuncs :: Eq a => [(a, [b])] -> [a -> Maybe b]
zipFuncs = map (flip lookup) . transposeRagged . map (uncurry zip . first (:[]))

encRegValues :: Proc -> Int -> [Text -> Maybe (V.Expr, V.Expr, V.Expr)]
encRegValues Proc{..} size =
  mapMaybe ((const . Just . (\(x,y) -> (V.Literal 1, x, y)) <$>) . encReg [] [] ([], ([], ConstBitsExpr []))) always
  ++ (zipFuncs . map (\(Inst n argTypes (ruleArgs, impls) enc) -> (n, mapMaybe (((\(x,y) -> (V.BinaryOp "execute" "&" (V.Variable $ "`is_inst_" ++ n ++ "(inst)"), x, y)) <$>) . encReg argTypes ruleArgs enc) impls)) $ insts)
  ++ (zipFuncs . map (\(Button n _ impls) -> (n, mapMaybe (((\(x,y) -> (V.RawExpr $ "!running & " ++ n ++ "_trigger", x, y)) <$>) . encReg [] [] ([], ([], ConstBitsExpr []))) impls)) $ buttons)
  where encReg :: [Type] -> [Text] -> ([Text], ([Bit], BitsExpr)) -> ImplRule -> Maybe (V.Expr, V.Expr)
        encReg argTypes ruleArgs enc                    (ImplRule (RegLValue r) e) =
          ((, genExpr argTypes ruleArgs enc e) <$>) . join . headMay . mapMaybe (\(Reg n _ regEnc) -> if n == r then Just (V.Bits <$> regEnc) else Nothing) $ regs
        encReg argTypes ruleArgs (encArgs, (bits, enc)) (ImplRule (VarLValue v) e) =
          case lookup v $ zip ruleArgs (zip argTypes encArgs) of
            Just (argType, encArg) ->
              case argType of
                RegT size'
                  | size == size' ->
                      case findVarInEnc encArg (length bits) enc of
                        Just (i, j) -> Just
                          ( V.Index
                            (V.Variable $ "_regs" ++ tshow size)
                            (V.Index "inst" (V.Literal j) (V.Literal i))
                            (V.Index "inst" (V.Literal j) (V.Literal i))
                          , genExpr argTypes ruleArgs (encArgs, (bits, enc)) e
                          )
                        Nothing     -> error $ "Variable " ++ unpack v ++ " not used in encoding"
                  | otherwise     -> Nothing
                BitsT _ -> error "Binary argument"
                IntT  _ -> error "Integer argument"
                InstT   -> error "Instruction argument"
            Nothing     -> error $ "No variable " ++ unpack v
        encReg _        _         _                      _                         = Nothing

genNonEncRegImpl :: [Inst] -> [Button] -> [ImplRule] -> Reg -> V.Verilog
genNonEncRegImpl insts buttons always (Reg name size _) = V.Seq
  [ V.Comment $ "Register: " ++ name
  , V.Wire
      size
      ("new_" ++ name)
      Nothing
      ( Just $ V.MultiCond
        ( V.MultiCond (V.RawExpr name)
            (maybeToList . genAlwaysRule regPred $ always)
        )
        (  mapMaybe (genInstRule regPred) insts
        ++ mapMaybe (genButtonRule regPred) buttons
        )
      )
  ]
  where regPred :: ImplRule -> Maybe Expr
        regPred (ImplRule (RegLValue reg) expr)
          | reg == name = Just expr
          | otherwise   = Nothing
        regPred  _      = Nothing

genNonEncRegImpls :: Proc -> V.Verilog
genNonEncRegImpls Proc{..} = V.Seq . map (genNonEncRegImpl insts buttons always) . filter (\(Reg _ _ e) -> null e) $ regs

genEncRegImpls :: Proc -> V.Verilog
genEncRegImpls Proc{..} = V.Seq
  [ V.Wire
      i
      ("_new_reg_" ++ tshow i ++ "_" ++ tshow j)
      Nothing
      (Just . V.MultiCond V.UndefinedBehaviour $ rs)
    | (i, j, rs) <- impls ]
  where impls :: [(Int, Integer, [(V.Expr, V.Expr)])]
        impls = concatMap (\size -> zipWith (size,,) [0..] . map (\f -> mapMaybe (\(Inst n _ _ _) -> (\(x, _, z) -> (x, z)) <$> f n) insts) . encRegValues Proc{..} $ size) sizes
        sizes :: [Int]
        sizes = Set.toList . Set.fromList . mapMaybe (\(Reg _ n e) -> e $> n) $ regs

genRegImpls :: Proc -> V.Verilog
genRegImpls = combineBlocks [genNonEncRegImpls, genEncRegImpls]

genEncRegIndices :: Proc -> V.Verilog
genEncRegIndices Proc{..} = V.Seq
  [ V.Wire
      i
      ("_index_reg_" ++ tshow i ++ "_" ++ tshow j)
      Nothing
      (Just . V.MultiCond V.UndefinedBehaviour $ rs)
    | (i, j, rs) <- impls ]
  where impls :: [(Int, Integer, [(V.Expr, V.Expr)])]
        impls = concatMap (\size -> zipWith (size,,) [0..] . map (\f -> mapMaybe (\(Inst n _ _ _) -> (\(x, y, _) -> (x, y)) <$> f n) insts) . encRegValues Proc{..} $ size) sizes
        sizes :: [Int]
        sizes = Set.toList . Set.fromList . map (\(Reg _ n _) -> n) $ regs

genEncRegWrites :: Proc -> V.Verilog
genEncRegWrites Proc{..} = V.Seq
  [ V.Wire
      i
      ("_write_reg_" ++ tshow i ++ "_" ++ tshow j)
      Nothing
      (Just . V.MultiCond (V.Literal 0) $ rs)
    | (i, j, rs) <- impls ]
  where impls :: [(Int, Integer, [(V.Expr, V.Expr)])]
        impls = concatMap (\size -> zipWith (size,,) [0..] . map (\f -> mapMaybe (\(Inst n _ _ _) -> (\(x, _, _) -> (x, V.Literal 1)) <$> f n) insts) . encRegValues Proc{..} $ size) sizes
        sizes :: [Int]
        sizes = Set.toList . Set.fromList . map (\(Reg _ n _) -> n) $ regs

genInitialiseRegs :: Map Text Int -> Proc -> V.Verilog
genInitialiseRegs regDefs =
  V.Initial
  . mapMaybe (\case
    Reg name _ Nothing  -> (Nothing, V.Variable name,) . V.Literal <$> Map.lookup name regDefs
    Reg name n (Just e) -> (Nothing, V.Index (V.Variable $ "__regs" ++ tshow n) (V.Bits e) (V.Bits e),) . V.Literal <$> Map.lookup name regDefs
    )
  . regs

genUpdateRegs :: Proc -> V.Verilog
genUpdateRegs Proc{..} =
  V.Always "posedge clk"
    ( [ ( Nothing
        , V.RawExpr name
        , V.RawExpr $ "new_" ++ name
        ) | Reg name _ e <- regs, null e] ++
      [ ( Just . V.Variable $ "_write_reg_" ++ tshow i ++ "_" ++ tshow j
        , V.RawExpr $ "_regs" ++ tshow i ++ "[" ++ "_index_reg_" ++ tshow i ++ "_" ++ tshow j ++ "]"
        , V.Variable $ "_new_reg_" ++ tshow i ++ "_" ++ tshow j
        ) | (i, j) <- impls ]
      )
  where impls :: [(Int, Int)]
        impls = concatMap (\size -> (\n -> [(size, i) | i <- [0..n-1]]) . length . encRegValues Proc{..} $ size) sizes
        sizes :: [Int]
        sizes = Set.toList . Set.fromList . map (\(Reg _ n _) -> n) $ regs

genMemoryOut :: Memory -> V.Verilog
genMemoryOut (Memory name dataWidth _) = V.Wire dataWidth (name ++ "_out") Nothing Nothing

genMemoryOuts :: Proc -> V.Verilog
genMemoryOuts = V.Seq . map genMemoryOut . memorys

genMemoryModule :: Map Text FilePath -> Memory -> V.Verilog
genMemoryModule memoryFiles (Memory name dataWidth addressWidth) =
  V.Module
    ("RAM_" ++ name)
    [ "input clk"
    , "input write"
    , "input [" ++ tshow (addressWidth - 1) ++ ":0] addr"
    , "input [" ++ tshow (dataWidth - 1) ++ ":0] in_data"
    , "output [" ++ tshow (dataWidth - 1) ++ ":0] out_data"
    ]
  . V.Seq
  $ [ V.Reg dataWidth "memorySpace" (Just $ 2 ^ addressWidth) Nothing
    , V.Reg dataWidth "data_out_reg" Nothing Nothing
    , V.Always "posedge clk"
      [ ( Just "write"
        , V.Index "memorySpace" "addr" "addr"
        , "in_data"
        )
      , ( Nothing
        , "data_out_reg"
        , V.Index "memorySpace" "addr" "addr"
        )
      ]
    , V.Assign "out_data" "data_out_reg"
    ] ++
    ( maybe
      []
      (\file ->
        [ V.RawVerilog . intercalate "\n" $
          [ "  initial begin"
          , "    $readmemh(" ++ tshow file ++ ", memorySpace);"
          , "  end"
          ]
        ]
      )
    . Map.lookup name
    $ memoryFiles 
    )

genMemoryModules :: Map Text FilePath -> Proc -> V.Verilog
genMemoryModules memoryFiles = V.Seq . map (genMemoryModule memoryFiles) . memorys

genMemoryRAM :: Memory -> [Text]
genMemoryRAM (Memory name _ _) =
  [ "  RAM_" ++ name
  , name
  , "(.clk(clk),"
  , ".write(" ++ name ++ "_write),"
  , ".addr(" ++ name ++ "_addr),"
  , ".in_data(" ++ name ++ "_in),"
  , ".out_data(" ++ name ++ "_out));"
  ]

genMemoryRAMs :: Proc -> V.Verilog
genMemoryRAMs = V.RawVerilog . combineLines' ' ' " " "\n" . map genMemoryRAM . memorys

genMemoryIn :: Proc -> Memory -> V.Verilog
genMemoryIn Proc{..} (Memory name dataWidth _) =
  V.Wire
    dataWidth
    (name ++ "_in")
    Nothing
    ( Just . V.MultiCond V.UndefinedBehaviour $
      (  mapMaybe (genInstRule memoryPred) insts
      ++ mapMaybe (genButtonRule memoryPred) buttons
      ++ maybeToList (genAlwaysRule memoryPred always)
      ))
  where memoryPred :: ImplRule -> Maybe Expr
        memoryPred (ImplRule (MemAccessLValue mem _) expr)
          | mem == name = Just expr
          | otherwise   = Nothing
        memoryPred  _   = Nothing

genMemoryIns :: Proc -> V.Verilog
genMemoryIns Proc{..} = V.Seq . map (genMemoryIn Proc{..}) $ memorys

genMemoryAddr :: Proc -> Memory -> V.Verilog
genMemoryAddr Proc{..} (Memory name _ addressWidth) =
  V.Wire
    addressWidth
    (name ++ "_addr")
    Nothing
    (Just . V.MultiCond V.UndefinedBehaviour $
      (  mapMaybe (genInstRule memoryPred) insts
      ++ mapMaybe (genButtonRule memoryPred) buttons
      ++ maybeToList (genAlwaysRule memoryPred always)
      ))
  where memoryPred :: ImplRule -> Maybe Expr
        memoryPred (ImplRule (MemAccessLValue mem expr) _)
          | mem == name = Just expr
          | otherwise   = memAccessForExpr expr
        memoryPred (ImplRule _ expr) = memAccessForExpr expr
        memAccessForExpr :: Expr -> Maybe Expr
        memAccessForExpr (VarExpr _ _)              = Nothing
        memAccessForExpr (RegExpr _ _)              = Nothing
        memAccessForExpr (MemAccessExpr _ n e)      = if n == name then Just e else memAccessForExpr e
        memAccessForExpr (ConstExpr _)              = Nothing
        memAccessForExpr (BinaryConstExpr _)        = Nothing
        memAccessForExpr (OpExpr _ _ e1 e2)         = case memAccessForExpr e1 of
          Just e1' -> Just e1'
          Nothing  -> memAccessForExpr e2
        memAccessForExpr (TernaryExpr _ b e1 e2)    = case memAccessForBoolExpr b of
          Just b' -> Just b'
          Nothing -> case memAccessForExpr e1 of
            Just e1' -> Just e1'
            Nothing  -> memAccessForExpr e2
        memAccessForBoolExpr :: BoolExpr -> Maybe Expr
        memAccessForBoolExpr (EqualityExpr   e1 e2) = case memAccessForExpr e1 of
          Just e1' -> Just e1'
          Nothing  -> memAccessForExpr e2
        memAccessForBoolExpr (InequalityExpr e1 e2) = case memAccessForExpr e1 of
          Just e1' -> Just e1'
          Nothing  -> memAccessForExpr e2
        memAccessForBoolExpr (LogicalAndExpr b1 b2) = case memAccessForBoolExpr b1 of
          Just b1' -> Just b1'
          Nothing  -> memAccessForBoolExpr b2
        memAccessForBoolExpr (LogicalOrExpr  b1 b2) = case memAccessForBoolExpr b1 of
          Just b1' -> Just b1'
          Nothing  -> memAccessForBoolExpr b2

genMemoryAddrs :: Proc -> V.Verilog
genMemoryAddrs Proc{..} = V.Seq . map (genMemoryAddr Proc{..}) $ memorys

genMemoryWrite :: Proc -> Memory -> V.Verilog
genMemoryWrite Proc{..} (Memory name _ _) =
  V.Wire
    1
    (name ++ "_write")
    Nothing
    (Just $ V.FoldR "|" (V.Literal 0)
      (  mapMaybe (genIsInstRule memoryPred) insts
      ++ mapMaybe (genIsButtonRule memoryPred) buttons
      ++ maybeToList (genIsAlwaysRule memoryPred always)
      ))
  where memoryPred :: ImplRule -> Maybe Expr
        memoryPred (ImplRule (MemAccessLValue mem expr) _)
          | mem == name = Just expr
          | otherwise   = Nothing
        memoryPred  _   = Nothing

genMemoryWrites :: Proc -> V.Verilog
genMemoryWrites Proc{..} = V.Seq . map (genMemoryWrite Proc{..}) $ memorys

genLed :: LedImpl -> V.Verilog
genLed (LedImpl n1 n2 e) =
  V.Assign
    (V.Index "led" (V.Literal n2) (V.Literal n1))
    (genExpr' e)

genLeds :: Proc -> V.Verilog
genLeds = V.Seq . map genLed . leds

genProcModule :: Map Text Int -> Proc -> V.Verilog
genProcModule regDefs = V.Module "PROCESSOR" ["input clk", "output [23:0] led", "output [3:0] indicators", "input [15:0] buttons"] . combineBlocks
  [ genButtonTriggers
  , genMemoryOuts
  , boilerplateRegs
  , genRegDecls
  , genRegImpls
  , genEncRegIndices
  , genEncRegWrites
  , genInitialiseRegs regDefs
  , genUpdateRegs
  , genMemoryIns
  , genMemoryAddrs
  , genMemoryWrites
  , genMemoryRAMs
  , genLeds
  ]

genAST :: Map Text FilePath -> Map Text Int -> Proc -> V.Verilog
genAST memoryFiles regDefs = combineBlocks [genPreamble, genMemoryModules memoryFiles, genProcModule regDefs]

-- | Generate the verilog code for the given processor
genVerilog :: Map Text FilePath -> Map Text Int -> Proc -> Text
genVerilog memoryFiles regDefs = output 0 . optimise . genAST memoryFiles regDefs

