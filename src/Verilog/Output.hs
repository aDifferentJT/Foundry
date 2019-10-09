{-# LANGUAGE TupleSections #-}

{-|
Module      : Verilog.AST
Description : An AST for Verilog
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Verilog.Output
  ( output
  ) where

import Verilog.AST

import Utils (mapInitLast)

import Data.List (intercalate, transpose)

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
align'  []    _ _   (_:_)  = error "ns shorter than ss"

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

output :: Int -> Verilog -> String
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
  ++ (if n == 1 then "" else "[" ++ show (n - 1) ++ ":0] ")
  ++ x
  ++ (case m of
    Nothing -> ""
    Just m' -> " [0:" ++ show (m' - 1) ++ "]"
    )
  ++ (case y of
    Nothing -> ""
    Just y' -> " = " ++ outputExpr (l + 1) False y'
    )
  ++ ";"
output l (Reg  n x m y)  =
     replicate (2 * l) ' '
  ++ "reg "
  ++ (if n == 1 then "" else "[" ++ show (n - 1) ++ ":0] ")
  ++ x
  ++ (case m of
    Nothing -> ""
    Just m' -> " [0:" ++ show (m' - 1) ++ "]"
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

outputExpr :: Int -> Bool -> Expr -> String
outputExpr _ _     (RawExpr s)         = s
outputExpr _ _     (Literal n)         = show n
outputExpr _ _     (Variable v)        = v
outputExpr _ _     (Bits bs)           = (show . length $ bs) ++ "'b" ++ concatMap show bs
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
outputExpr l b     (FoldR o a [])      = outputExpr l b a
outputExpr l b     (FoldR o a [x])     = outputExpr l b x
outputExpr l False (FoldR o a xs)      =
  if null xs
  then
    outputExpr l False a
  else
    combineLines ' ' " " "" . mapInitLast (, o) (, "") . map (\x -> p ++ outputExpr (l + 1) True x) $ xs
  where p = "\n" ++ replicate (2 * l) ' ' 
outputExpr _ _      UndefinedBehaviour = "0"
outputExpr l True   e                  = "(" ++ outputExpr l False e ++ ")"

