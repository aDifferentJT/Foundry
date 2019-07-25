{-# LANGUAGE RecordWildCards, LambdaCase #-}

module CodeGen.CodeGen
  ( genCode
  ) where

import Proc
import Utils (Bit(..))

import Control.Arrow
import Data.List
import Data.Maybe

padRightTo :: Int -> a -> [a] -> [a]
padRightTo n x  []    = replicate n x
padRightTo n x (y:ys) = y : padRightTo (n - 1) x ys

align :: Int -> a -> [a] -> ([a], [a]) -> [a]
align _ _ _   (s, [])  = s
align n p sep (s1, s2) = padRightTo n p s1 ++ sep ++ s2

align' :: [Int] -> a -> [a] -> [[a]] -> [a]
align'  _     _ _    []    = []
align'  _     _ _    [s]   = s
align' (n:ns) p sep (s:ss) = padRightTo n p s ++ sep ++ align' ns p sep ss
align'  []    _ _   (s:ss) = error "ns shorter than ss"

combineBlocks :: [a -> String] -> a -> String
combineBlocks fs = intercalate "\n\n" . flip fmap fs . flip ($)

alignLines :: a -> [a] -> [([a], [a])] -> [[a]]
alignLines p colSep ls = map (align indent p colSep) ls
  where indent :: Int
        indent = maximum . map (length . fst) $ ls

combineLines :: a -> [a] -> [a] -> [([a], [a])] -> [a]
combineLines p colSep lineSep = intercalate lineSep . alignLines p colSep

alignLines' :: a -> [a] -> [[[a]]] -> [[a]]
alignLines' p colSep ls = map (align' indents p colSep) ls
  where indents :: [Int]
        indents = map (maximum . map length) . transpose $ ls

combineLines' :: a -> [a] -> [a] -> [[[a]]] -> [a]
combineLines' p colSep lineSep = intercalate lineSep . alignLines' p colSep

genBits :: [Bit] -> String
genBits bs = (show . length $ bs) ++ "'b" ++ (concat . map show $ bs)

genInstDef :: Inst -> [String]
genInstDef (Inst n _ _ (_, (bs, e))) = 
  [ "`define is_inst_" ++ n ++ "(inst)"
  , "(inst[" ++ show (length bs) ++ ":0]"
  , "=="
  , genBits bs ++ ")"
  ]

genInstDefs :: Proc -> String
genInstDefs = combineLines' ' ' " " "\n" . map genInstDef . insts

genButtonDef :: Button -> (String, String)
genButtonDef (Button n i _) =
  ( "`define BUTTON_" ++ n
  , "buttons[" ++ show i ++ "]"
  )

genButtonDefs :: Proc -> String
genButtonDefs = combineLines ' ' " " "\n" . map genButtonDef . buttons

genDefs :: Proc -> String
genDefs = combineBlocks [genInstDefs, genButtonDefs]

includes :: Proc -> String
includes = const . intercalate "\n" $
  [ "`include \"ram.v\""
  , "`include \"prescaler.v\""
  , "`include \"single_trigger.v\""
  ]

genPreamble :: Proc -> String
genPreamble = combineBlocks [includes, genDefs]

startProcModule :: Proc -> String
startProcModule = const "module PROCESSOR (input clk, output [23:0] led, output [3:0] indicators, input [15:0] buttons);"

endModule :: Proc -> String
endModule = const "endmodule"

boilerplateRegs :: Proc -> String
boilerplateRegs = combineBlocks . map (const . intercalate "\n") $
  [ [ "  // Handle running"
    , "  reg running = 0;"
    , "  always @ (posedge `RUN_BUTTON) begin"
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
    , "    if (execute & `is_inst_halt)"
    , "      halt <= 1;"
    , "    if (!running)"
    , "      halt <= 0;"
    , "  end "
    ]
  ]

genButtonWire :: Button -> String
genButtonWire (Button name i _) = "  wire " ++ name ++ "_trigger;"

genButtonTrigger :: Button -> [String]
genButtonTrigger (Button name i _) =
  [ "  SINGLE_TRIGGER trig_" ++ name
  , "(.clk(clk), .trigger_in(`BUTTON_" ++ name ++ "),"
  , ".trigger_out(" ++ name ++ "_trigger));"
  ]

genButtonTriggers :: Proc -> String
genButtonTriggers = combineBlocks
  [ intercalate "\n" . map genButtonWire . buttons
  , combineLines' ' ' " " "\n" . map genButtonTrigger . buttons
  ]

genOp :: Op -> String
genOp Add        = "+"
genOp Sub        = "-"
genOp Mul        = "*"
genOp Div        = "/"
genOp ConcatBits = "++"
genOp BitwiseAnd = "&"
genOp BitwiseOr  = "|"
genOp BitwiseXor = "^"

genBoolExpr :: ([String], [ImplRule]) -> ([String], ([Bit], BitsExpr)) -> BoolExpr -> String
genBoolExpr (ruleArgs, rules) (encArgs, (bits, enc)) (EqualityExpr e1 e2)   = intercalate " " $
  [ genExpr (ruleArgs, rules) (encArgs, (bits, enc)) e1
  , " == "
  , genExpr (ruleArgs, rules) (encArgs, (bits, enc)) e2
  ]
genBoolExpr (ruleArgs, rules) (encArgs, (bits, enc)) (LogicalAndExpr b1 b2) = intercalate " " $
  [ genBoolExpr (ruleArgs, rules) (encArgs, (bits, enc)) b1
  , " && "
  , genBoolExpr (ruleArgs, rules) (encArgs, (bits, enc)) b2
  ]
genBoolExpr (ruleArgs, rules) (encArgs, (bits, enc)) (LogicalOrExpr b1 b2) = intercalate " " $
  [ genBoolExpr (ruleArgs, rules) (encArgs, (bits, enc)) b1
  , " || "
  , genBoolExpr (ruleArgs, rules) (encArgs, (bits, enc)) b2
  ]

genExpr :: ([String], [ImplRule]) -> ([String], ([Bit], BitsExpr)) -> Expr -> String
genExpr (ruleArgs, rules) (encArgs, (bits, enc)) (VarExpr var)         =
  case lookup var $ zip ruleArgs encArgs of
    Just encArg ->
      case findVarInEnc encArg (length bits) enc of
        Just (i, j) -> "inst[" ++ show j ++ ":" ++ show i ++ "]"
        Nothing     -> error $ "Variable " ++ var ++ " not used in encoding"
    Nothing     -> error $ "No variable " ++ var
genExpr (ruleArgs, rules) (encArgs, (bits, enc)) (RegExpr reg)         = reg
genExpr (ruleArgs, rules) (encArgs, (bits, enc)) (MemAccessExpr mem _) = mem ++ "_out"
genExpr (ruleArgs, rules) (encArgs, (bits, enc)) (ConstExpr n)         = show n
genExpr (ruleArgs, rules) (encArgs, (bits, enc)) (BinaryConstExpr bs)  = genBits bs
genExpr (ruleArgs, rules) (encArgs, (bits, enc)) (OpExpr o e1 e2)      = intercalate " " $ 
  [ genExpr (ruleArgs, rules) (encArgs, (bits, enc)) e1
  , genOp o
  , genExpr (ruleArgs, rules) (encArgs, (bits, enc)) e2
  ]
genExpr (ruleArgs, rules) (encArgs, (bits, enc)) (TernaryExpr b e1 e2) = intercalate " " $
  [ genBoolExpr (ruleArgs, rules) (encArgs, (bits, enc)) b
  , "?"
  , genExpr (ruleArgs, rules) (encArgs, (bits, enc)) e1
  , ":"
  , genExpr (ruleArgs, rules) (encArgs, (bits, enc)) e2
  ]

genInstRule :: (ImplRule -> Maybe Expr) -> Inst -> Maybe ([String], String)
genInstRule pred (Inst name _ (ruleArgs, rules) (encArgs, (bits, enc))) =
  case mapMaybe pred rules of
    []      -> Nothing
    [expr]  -> Just
      ( [ "    execute & `is_inst_" ++ name ++ "(inst)"
        , "?"
        , genExpr (ruleArgs, rules) (encArgs, (bits, enc)) expr
        , ":"
        ]
      , "Instruction: " ++ name
      )
    (_:_:_) -> error "More than one rule"

genIsInstRule :: (ImplRule -> Maybe Expr) -> Inst -> Maybe (String, String)
genIsInstRule pred (Inst name _ (ruleArgs, rules) (encArgs, (bits, enc))) =
  case mapMaybe pred rules of
    []      -> Nothing
    [expr]  -> Just
      ( "    (execute & `is_inst_" ++ name ++ "(inst)) |"
      , "Instruction: " ++ name
      )
    (_:_:_) -> error "More than one rule"

genButtonRule :: (ImplRule -> Maybe Expr) -> Button -> Maybe ([String], String)
genButtonRule pred (Button name _ rules) =
  case mapMaybe pred rules of
    []      -> Nothing
    [expr]  -> Just
      ( [ "    !running & " ++ name ++ "_trigger"
        , "?"
        , genExpr ([], rules) ([], ([], ConstBitsExpr [])) expr
        , ":"
        ]
      , "Button: " ++ name
      )
    (_:_:_) -> error "More than one rule"

genIsButtonRule :: (ImplRule -> Maybe Expr) -> Button -> Maybe (String, String)
genIsButtonRule pred (Button name _ rules) =
  case mapMaybe pred rules of
    []      -> Nothing
    [expr]  -> Just
      ( "    (!running & " ++ name ++ "_trigger) |"
      , "Button: " ++ name
      )
    (_:_:_) -> error "More than one rule"

genReg :: [Inst] -> [Button] -> Reg -> String
genReg insts buttons (Reg name size enc) = combineLines ' ' " // " "\n" $
  [ ("  // Register: " ++ name, "")
  , ("  reg [" ++ show (size - 1) ++ ":0] " ++ name ++ " = 0;", "")
  , ("  wire [" ++ show (size - 1) ++ ":0] new_" ++ name ++ " =", "")
  ] ++
  (uncurry zip . first (alignLines' ' ' " ") . unzip . mapMaybe (genInstRule regPred) $ insts) ++
  (uncurry zip . first (alignLines' ' ' " ") . unzip . mapMaybe (genButtonRule regPred) $ buttons) ++
  (if name == "pc" then [("    execute & pc + 1;", "Increment PC")] else []) ++
  [ ("    " ++ name ++ ";", "Fallthrough to keep it the same")
  ]
  where regPred :: ImplRule -> Maybe Expr
        regPred (ImplRule (RegLValue reg) expr)
          | reg == name = Just expr
          | otherwise   = Nothing
        regPred  _      = Nothing

genRegs :: Proc -> String
genRegs Proc{..} = intercalate "\n\n" . map (genReg insts buttons) $ regs

genMemoryOut :: Memory -> [String]
genMemoryOut (Memory name dataWidth addressWidth) =
  [ "  wire [" ++ show (dataWidth - 1) ++ ":0]"
  , name ++ "_out;"
  ]

genMemoryOuts :: Proc -> String
genMemoryOuts = combineLines' ' ' " " "\n" . map genMemoryOut . memorys

genMemoryRAM :: Memory -> [String]
genMemoryRAM (Memory name dataWidth addressWidth) =
  [ "  RAM #(.DATA_BITS(" ++ show dataWidth ++ "),.ADDRESS_BITS(" ++ show addressWidth ++ "))"
  , name
  , "(.clk(clk),"
  , ".write(" ++ name ++ "_write),"
  , ".addr(" ++ name ++ "_addr),"
  , ".in_data(" ++ name ++ "_in),"
  , ".out_data(" ++ name ++ "_out));"
  ]

genMemoryRAMs :: Proc -> String
genMemoryRAMs = combineLines' ' ' " " "\n" . map genMemoryRAM . memorys

genMemoryIn :: [Inst] -> [Button] -> Memory -> String
genMemoryIn insts buttons (Memory name dataWidth addressWidth) = combineLines ' ' " // " "\n" $
  [ ("  // Memory: " ++ name, "")
  , ("  wire [" ++ show (dataWidth - 1) ++ ":0] " ++ name ++ "_in =", "")
  ] ++
  (uncurry zip . first (alignLines' ' ' " ") . unzip . mapMaybe (genInstRule memoryPred) $ insts) ++
  (uncurry zip . first (alignLines' ' ' " ") . unzip . mapMaybe (genButtonRule memoryPred) $ buttons) ++
  [ ("    0;", "Fallthrough, should never be used")
  ]
  where memoryPred :: ImplRule -> Maybe Expr
        memoryPred (ImplRule (MemAccessLValue mem _) expr)
          | mem == name = Just expr
          | otherwise   = Nothing
        memoryPred  _   = Nothing

genMemoryIns :: Proc -> String
genMemoryIns Proc{..} = intercalate "\n\n" . map (genMemoryIn insts buttons) $ memorys

genMemoryAddr :: [Inst] -> [Button] -> Memory -> String
genMemoryAddr insts buttons (Memory name dataWidth addressWidth) = combineLines ' ' " // " "\n" $
  [ ("  // Memory: " ++ name, "")
  , ("  wire [" ++ show (addressWidth - 1) ++ ":0] " ++ name ++ "_addr =", "")
  ] ++
  (uncurry zip . first (alignLines' ' ' " ") . unzip . mapMaybe (genInstRule memoryPred) $ insts) ++
  (uncurry zip . first (alignLines' ' ' " ") . unzip . mapMaybe (genButtonRule memoryPred) $ buttons) ++
  [ ("    0;", "Fallthrough, should never be used")
  ]
  where memoryPred :: ImplRule -> Maybe Expr
        memoryPred (ImplRule (MemAccessLValue mem expr) _)
          | mem == name = Just expr
          | otherwise   = Nothing
        memoryPred  _   = Nothing

genMemoryAddrs :: Proc -> String
genMemoryAddrs Proc{..} = intercalate "\n\n" . map (genMemoryAddr insts buttons) $ memorys

genMemoryWrite :: [Inst] -> [Button] -> Memory -> String
genMemoryWrite insts buttons (Memory name dataWidth addressWidth) = combineLines ' ' " // " "\n" $
  [ ("  // Memory: " ++ name, "")
  , ("  wire [" ++ show (addressWidth - 1) ++ ":0] " ++ name ++ "_write =", "")
  ] ++
  (mapMaybe (genIsInstRule memoryPred) insts) ++
  (mapMaybe (genIsButtonRule memoryPred) buttons) ++
  [ ("    0;", "Fallthrough, default to not writing")
  ]
  where memoryPred :: ImplRule -> Maybe Expr
        memoryPred (ImplRule (MemAccessLValue mem expr) _)
          | mem == name = Just expr
          | otherwise   = Nothing
        memoryPred  _   = Nothing

genMemoryWrites :: Proc -> String
genMemoryWrites Proc{..} = intercalate "\n\n" . map (genMemoryWrite insts buttons) $ memorys

genUpdateInsts :: Proc -> String
genUpdateInsts Proc{..} = intercalate "\n" $
  [ "  always @ (posedge clk) begin"
  ] ++
  alignLines' ' ' " "
    [
      [ "    " ++ name
      , "<="
      , "new_" ++ name
      ] | (Inst name _ _ _) <- insts] ++
  [ "  end"
  ]

genProcModule :: Proc -> String
genProcModule = combineBlocks
  [ startProcModule
  , genButtonTriggers
  , genMemoryOuts
  , boilerplateRegs
  , genRegs
  , genUpdateInsts
  , genMemoryIns
  , genMemoryAddrs
  , genMemoryWrites
  , genMemoryRAMs
  , endModule
  ]

genCode :: Proc -> String
genCode = (++ "\n\n") . combineBlocks [genPreamble, genProcModule]

