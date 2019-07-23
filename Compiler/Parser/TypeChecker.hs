module TypeChecker
  ( typeCheck
  ) where

import AST

import Control.Applicative (liftA2)
import Control.Monad (unless)
import Control.Monad.Except
import Data.List (intercalate)
import Data.Maybe (listToMaybe)

checkInstArgsMatch :: Inst -> Either String ()
checkInstArgsMatch (Inst n ts (vs1, rs) (vs2, e)) = do
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

instEncDim :: [EncType] -> Inst -> Either String Int
instEncDim _        (Inst _ _  _  (_,  ConstBitsExpr bs))     = return $ length bs
instEncDim encTypes (Inst _ ts _  (vs, EncBitsExpr v))  = case filter ((== v) . fst) $ zip vs ts of
  []       -> throwError $ "No such variable " ++ v
  [(_, t)] -> getEncType encTypes t
  _        -> throwError $ "Two variables with the same name " ++ v
instEncDim encTypes (Inst n ts rs (vs, ConcatBitsExpr e1 e2)) = liftA2 (+) (instEncDim encTypes (Inst n ts rs (vs, e1))) (instEncDim encTypes (Inst n ts rs (vs, e2)))

checkRegEncDim :: [EncType] -> Reg -> Either String ()
checkRegEncDim encTypes (Reg n t bs) = do
  d1 <- getEncType encTypes (RegT t)
  let d2 = length bs
  unless (d1 == d2) . throwError $ "<" ++ n ++ "> is of type Bits " ++ show d2 ++ " but I expected Bits " ++ show d1

checkInstEncDim :: [EncType] -> Inst -> Either String ()
checkInstEncDim encTypes (Inst n ts rs (vs, e)) = do
  d1 <- getEncType encTypes InstT
  d2 <- instEncDim encTypes (Inst n ts rs (vs, e))
  unless (d1 == d2) . throwError $ "<" ++ intercalate " " (n : ["<" ++ v ++ ">" | v <- vs]) ++ "> is of type Bits " ++ show d2 ++ " but I expected Bits " ++ show d1

typeCheck :: Proc -> Either String Proc
typeCheck (Proc regs insts encTypes) = do
  mapM_ (checkRegEncDim encTypes) regs
  mapM_ checkInstArgsMatch insts
  mapM_ (checkInstEncDim encTypes) insts
  return (Proc regs insts encTypes)

