{-# LANGUAGE LambdaCase #-}

module CodeGen.CodeGen
  ( genCode
  ) where

import Proc
import Utils (Bit(..))

import Data.List
import Data.Maybe

combine :: [a -> String] -> a -> String
combine fs = intercalate "\n\n" . flip fmap fs . flip ($)

genBits :: [Bit] -> String
genBits bs = (show . length $ bs) ++ "'b" ++ (concat . map show $ bs)

{-
genInstDef :: Inst -> String
genInstDef (Inst n _ _ (_, (bs, e))) = intercalate "\n"
  [ "`define INST_" ++ n ++ " " ++ genBits bs
  ]

genInstDefs :: Proc -> String
genInstDefs (Proc _ insts _ _ _) = intercalate "\n" . map genInstDef $ insts
-}

genButtonDef :: Button -> String
genButtonDef (Button n i _) = "`define BUTTON_" ++ n ++ " buttons[" ++ show i ++ "]"

genButtonDefs :: Proc -> String
genButtonDefs (Proc _ _ buttons _ _) = intercalate "\n" . map genButtonDef $ buttons

genDefs :: Proc -> String
genDefs = genButtonDefs --combine [genInstDefs, genButtonDefs]

includes :: Proc -> String
includes = const . intercalate "\n" $
  [ "`include \"ram.v\""
  ]

genPreamble :: Proc -> String
genPreamble = combine [includes, genDefs]

startProcModule :: Proc -> String
startProcModule = const "module PROCESSOR (input clk, output [23:0] led, output [3:0] indicators, input [15:0] buttons);"

endModule :: Proc -> String
endModule = const "endmodule"

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
      case findVarInEnc encArg enc of
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

genInstRule :: String -> Inst -> Maybe String
genInstRule reg (Inst name _ (ruleArgs, rules) (encArgs, (bits, enc))) =
  case filter (\case ImplRule (RegLValue reg') _ -> reg == reg'; _ -> False) rules of
    []                -> Nothing
    [ImplRule _ expr] -> Just $
      "  execute & (inst[" ++ show (length bits - 1) ++ ":0] == " ++ genBits bits ++ ") ? " ++ genExpr (ruleArgs, rules) (encArgs, (bits, enc)) expr ++ " : // Instruction: " ++ name
    (_:_:_)           -> error "More than one rule"

genButtonRule :: String -> Button -> Maybe String
genButtonRule reg (Button name _ rules) =
  case filter (\case ImplRule (RegLValue reg') _ -> reg == reg'; _ -> False) rules of
    []                -> Nothing
    [ImplRule _ expr] -> Just $
      "  !running & " ++ name ++ "_trigger ? " ++ genExpr ([], rules) ([], ([], ConstBitsExpr [])) expr ++ " : // Button: " ++ name
    (_:_:_)           -> error "More than one rule"

genReg :: [Inst] -> [Button] -> Reg -> String
genReg insts buttons (Reg name size enc) = intercalate "\n" $
  [ "// Register: " ++ name
  , "reg [" ++ show (size - 1) ++ ":0] " ++ name ++ " = 0;"
  , "wire [" ++ show (size - 1) ++ ":0] new_" ++ name ++ " ="
  ] ++
  mapMaybe (genInstRule name) insts ++
  mapMaybe (genButtonRule name) buttons ++
  (if name == "pc" then ["  execute & pc + 1;"] else []) ++
  [ "  " ++ name ++ ";"
  ]

genRegs :: Proc -> String
genRegs (Proc regs insts buttons _ _) = intercalate "\n\n" . map (genReg insts buttons) $ regs

genProcModule :: Proc -> String
genProcModule = combine [startProcModule, genRegs, endModule]

genCode :: Proc -> String
genCode = (++ "\n\n") . combine [genPreamble, genProcModule]

