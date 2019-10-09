{-|
Module      : Verilog.Optimiser
Description : A simple Verilog optimiser
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Verilog.Optimiser
  ( optimise
  ) where

import Verilog.AST

import Utils (groupWith, selectLargestBy)

import Control.Arrow ((***))

optimise :: Verilog -> Verilog
optimise (RawVerilog s)  = RawVerilog s
optimise (Comment s)     = Comment s
optimise (Seq xs)        = Seq . map optimise $ xs
optimise (Include x)     = Include x
optimise (Define x ys z) = Define x ys z
optimise (Module x ys z) = Module x ys (optimise z)
optimise (Wire n x m y)  = Wire n x m (simplify <$> y)
optimise (Reg  n x m y)  = Reg  n x m (simplify <$> y)
optimise (Always x ys)   = Always x (map (\(x, y, z) -> (simplify <$> x, simplify y, simplify z)) ys)
optimise (Assign x y)    = Assign (simplify x) (simplify y)

simplify :: Expr -> Expr
simplify e
  | e == e'   = e
  | otherwise = simplify e'
  where e' = simplifyOnce e

simplifyOnce :: Expr -> Expr
simplifyOnce (RawExpr s)                           = RawExpr s
simplifyOnce (Literal n)                           = Literal n
simplifyOnce (Variable v)                          = Variable v
simplifyOnce (Bits bs)                             = Bits bs
simplifyOnce (UnaryOp o x)                         = UnaryOp o (simplify x)
simplifyOnce (BinaryOp x o y)                      = BinaryOp (simplify x) o (simplify y)
simplifyOnce (TernaryOp _ y UndefinedBehaviour)    = simplify y
simplifyOnce (TernaryOp _ UndefinedBehaviour z)    = simplify z
simplifyOnce (TernaryOp x (Literal 1) (Literal 0)) = simplify x
simplifyOnce (TernaryOp x y z)                     = TernaryOp (simplify x) (simplify y) (simplify z)
simplifyOnce (MultiCond d [])                      = simplify d
simplifyOnce (MultiCond d [(c, e)])                = simplify $ TernaryOp (simplify c) (simplify e) (simplify d)
simplifyOnce (MultiCond UndefinedBehaviour es)     =
  let es'    = groupWith snd es in
  let (d', es'')    = selectLargestBy (length . snd) es' in
  MultiCond (simplify . maybe UndefinedBehaviour fst $ d') (map (\(y, xs) -> (simplify . FoldR "|" (Literal 0) . map fst $ xs, simplify y)) es'')
simplifyOnce (MultiCond d es)                      =
  let es'    = filter ((/= d) . fst) . groupWith snd $ es in
  MultiCond (simplify d) (map (\(y, xs) -> (simplify . FoldR "|" (Literal 0) . map fst $ xs, simplify y)) es')
simplifyOnce (FoldR _ a [])                        = simplify a
simplifyOnce (FoldR _ _ [x])                       = simplify x
simplifyOnce (FoldR o a xs)                        = FoldR o (simplify a) (map simplify xs)
simplifyOnce UndefinedBehaviour                    = UndefinedBehaviour

