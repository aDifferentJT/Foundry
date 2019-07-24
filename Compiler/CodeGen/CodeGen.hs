module CodeGen.CodeGen
  ( genCode
  ) where

import Proc
import Utils (Bit(..))

import Data.List

genBits :: [Bit] -> String
genBits bs = (show . length $ bs) ++ "'b" ++ (concat . map show $ bs)

genInstDef :: Inst -> String
genInstDef (Inst n _ _ (_, (bs, e))) = "`define INST_" ++ n ++ " " ++ genBits bs ++ "\n"

genInstDefs :: Proc -> String
genInstDefs (Proc _ insts _ _ _) = concat . map genInstDef $ insts

genButtonDef :: Button -> String
genButtonDef (Button n i _) = "`define BUTTON_" ++ n ++ " buttons[" ++ show i ++ "]\n"

genButtonDefs :: Proc -> String
genButtonDefs (Proc _ _ buttons _ _) = concat . map genButtonDef $ buttons

genDefs :: Proc -> String
genDefs = intercalate "\n" . flip map [genInstDefs, genButtonDefs] . flip ($)

genCode :: Proc -> String
genCode = genDefs

