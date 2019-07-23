module TypeChecker
  ( typeCheck
  ) where

import AST
import Utils ((****))

import Control.Applicative (liftA2)
import Control.Arrow
import Control.Monad (unless)
import Control.Monad.Except
import Data.List (intercalate)
import Data.Maybe (listToMaybe)

checkInstArgsMatch :: UnsizedInst -> Either String ()
checkInstArgsMatch (UnsizedInst n ts (vs1, _) (vs2, _)) = do
  let d1 = length ts
  let d2 = length vs1
  let d3 = length vs2
  unless (d1 == d2) . throwError $ "The implementation of " ++ n ++ " has " ++ show d2 ++ " arguments, " ++ show d1 ++ " expected"
  unless (d1 == d3) . throwError $ "The encoding of " ++ n ++ " has " ++ show d3 ++ " arguments, " ++ show d1 ++ " expected"

getEncType :: [EncType] -> Type -> Either String Int
getEncType _        (BitsT n) = return n
getEncType _        (IntT n)  = return n
getEncType encTypes  t        = case filter (\(EncType t2 _) -> t2 == t) $ encTypes of
  []            -> throwError $ "No type given for " ++ show t
  [EncType _ n] -> return n
  _             -> throwError $ "More than one type given for " ++ show t

instEncDim :: [EncType] -> [Type] -> [String] -> UnsizedBitsExpr -> Either String (Int, BitsExpr)
instEncDim _        _  _  (UnsizedConstBitsExpr bs)     = return (length bs, ConstBitsExpr bs)
instEncDim encTypes ts vs (UnsizedEncBitsExpr v)        = case filter ((== v) . fst) $ zip vs ts of
  []       -> throwError $ "No such variable " ++ v
  [(_, t)] -> (\n -> (n, EncBitsExpr n v)) <$> getEncType encTypes t
  _        -> throwError $ "Two variables with the same name " ++ v
instEncDim encTypes ts vs (UnsizedConcatBitsExpr e1 e2) = liftA2 ((+) **** ConcatBitsExpr) (instEncDim encTypes ts vs e1) (instEncDim encTypes ts vs e2)

checkRegEncDim :: [EncType] -> Reg -> Either String ()
checkRegEncDim encTypes (Reg n t bs) = do
  d1 <- getEncType encTypes (RegT t)
  let d2 = length bs
  unless (d1 == d2) . throwError $ "<" ++ n ++ "> is of type Bits " ++ show d2 ++ " but I expected Bits " ++ show d1

checkInstEncDim :: [EncType] -> UnsizedInst -> Either String Inst
checkInstEncDim encTypes (UnsizedInst n ts rs (vs, (bs, e))) = do
  d1 <- getEncType encTypes InstT
  (d2, e') <- instEncDim encTypes ts vs e
  unless (d1 == length bs + d2) . throwError $ "<" ++ intercalate " " (n : ["<" ++ v ++ ">" | v <- vs]) ++ "> is of type Bits " ++ show d2 ++ " but I expected Bits " ++ show d1
  return $ Inst n ts rs (vs, (bs, e'))

typeCheck :: UnsizedProc -> Either String Proc
typeCheck (UnsizedProc regs insts encTypes) = do
  mapM_ (checkRegEncDim encTypes) regs
  mapM_ checkInstArgsMatch insts
  insts' <- mapM (checkInstEncDim encTypes) insts
  return (Proc regs insts' encTypes)

