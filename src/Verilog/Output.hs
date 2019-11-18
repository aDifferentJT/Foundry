{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections #-}
{-|
Module      : Verilog.Output
Description : Convert the Verilog AST to Text
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Verilog.Output
  ( output
  ) where

import ClassyPrelude

import Verilog.AST
import Verilog.Align

import Utils (mapInitLast)

output :: Int -> Verilog -> Text
output _ (RawVerilog s)  = s
output l (Comment s)     = replicate (2 * l) ' ' ++ "// " ++ s
output l (Seq xs)        = unlines . map (output l) $ xs
output l (Include x)     = replicate (2 * l) ' ' ++ "`include \"" ++ x ++ "\""
output l (Define x ys z) = replicate (2 * l) ' '
  ++ "`define " ++ x ++ (if null ys then "" else "(" ++ intercalate "," ys ++ ")") ++ " " ++ z
output l (Module x ys z) =
     replicate (2 * l) ' '
  ++ "module " ++ x ++ " (" ++ intercalate "," ys ++ ");"
  ++ "\n"
  ++ output (l + 1) z
  ++ "\n"
  ++ "endmodule"
output l (Wire n x m y)  =
     replicate (2 * l) ' '
  ++ "wire "
  ++ (if n == 1 then "" else "[" ++ tshow (n - 1) ++ ":0] ")
  ++ x
  ++ (case m of
    Nothing -> ""
    Just m' -> " [0:" ++ tshow (m' - 1) ++ "]"
    )
  ++ (case y of
    Nothing -> ""
    Just y' -> " = " ++ outputExpr (l + 1) False y'
    )
  ++ ";"
output l (Reg  n x m y)  =
     replicate (2 * l) ' '
  ++ "reg "
  ++ (if n == 1 then "" else "[" ++ tshow (n - 1) ++ ":0] ")
  ++ x
  ++ (case m of
    Nothing -> ""
    Just m' -> " [0:" ++ tshow (m' - 1) ++ "]"
    )
  ++ (case y of
    Nothing -> ""
    Just y' -> " = " ++ outputExpr (l + 1) False y'
    )
  ++ ";" 
output l (Always x ys)   =
     replicate (2 * l) ' ' ++ "always @ (" ++ x ++ ") begin"
  ++ "\n"
  ++ combineLines ' ' " <= " "\n"
       [ ( replicate (2 * (l + 1)) ' ' ++ maybe "" (\c' -> "if (" ++ outputExpr (l + 1) False c' ++ ") ") c ++ outputExpr (l + 1) False y1
         , outputExpr (l + 1) False y2 ++ ";"
         )
       | (c, y1, y2) <- ys ]
  ++ "\n"
  ++ replicate (2 * l) ' ' ++ "end"
  ++ "\n"
output l (Assign x y)    =
     replicate (2 * l) ' '
  ++ "assign "
  ++ outputExpr (l + 1) False x
  ++ " = "
  ++ outputExpr (l + 1) False y
  ++ ";"

outputExpr :: Int -> Bool -> Expr -> Text
outputExpr _ _     (RawExpr s)         = s
outputExpr _ _     (Literal n)         = tshow n
outputExpr _ _     (Variable v)        = v
outputExpr _ _     (Bits bs)           = (tshow . length $ bs) ++ "'b" ++ concatMap tshow bs
outputExpr l _     (UnaryOp o x)       = o ++ outputExpr l True x
outputExpr l False (BinaryOp x o y)    = outputExpr l True x ++ " " ++ o ++ " " ++ outputExpr l True y
outputExpr l False (TernaryOp x y z)   =
     outputExpr l True x
  ++ " ? "
  ++ outputExpr l True y
  ++ " : "
  ++ outputExpr l False z
outputExpr l b     (MultiCond d [])    = outputExpr l b d
outputExpr l False (MultiCond d es)    =
  combineLines' ' ' " " "" [ [p ++ outputExpr (l + 1) True c, "?", outputExpr (l + 1) True e, ":"] | (c, e) <- es]
  ++ p
  ++ outputExpr (l + 1) False d
  where p = "\n" ++ replicate (2 * l) ' ' 
outputExpr l b     (FoldR _ a [])      = outputExpr l b a
outputExpr l b     (FoldR _ _ [x])     = outputExpr l b x
outputExpr l False (FoldR o _ xs)      =
  combineLines ' ' " " ""
  . mapInitLast (, o) (, "")
  . map (\x -> p ++ outputExpr (l + 1) True x)
  $ xs
  where p = "\n" ++ replicate (2 * l) ' ' 
outputExpr _ _      UndefinedBehaviour = "0"
outputExpr l True   e                  = "(" ++ outputExpr l False e ++ ")"

