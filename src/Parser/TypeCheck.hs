{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Parser.TypeCheck (typeCheck) where

import Parser.AST
import Parser.Monad

import Utils (fst3, zipBy, zip3By)

import Data.Maybe (mapMaybe)

typeCheck :: RawProc -> ParserMonad Proc
typeCheck RawProc{..} = do
  regs' <- case _rawRegs of
    []  -> throwGlobalError "No register block"
    [x] -> return x
    _   -> throwGlobalError "More than one register block"
  let regEncs = mapMaybe (\case RegEnc n e -> Just (n, e); _ -> Nothing) _rawEncs
  regs <- case zipBy (\(RegType n _) -> n) fst regs' regEncs of
    (xs, ys, []      ) -> return $ [Reg n t (Just e)  | (RegType n t, (_, e)) <- xs] ++ [Reg n t Nothing | (RegType n t) <- ys]
    (_ , _ , (n, _):_) -> throwGlobalError $ "Encoding given for unknown register " ++ n
  insts' <- case _rawInsts of
             []  -> throwGlobalError "No instruction block"
             [x] -> return x
             _   -> throwGlobalError "More than one instruction block"
  let instEncs = mapMaybe (\case InstEnc n vs e -> Just (n, vs, e); _ -> Nothing) _rawEncs
  let instImpls = mapMaybe (\case InstImpl "always" _ _ -> Nothing; InstImpl n vs rs -> Just (n, vs, rs); _ -> Nothing) _rawImpls
  let instName n vs = unwords (n : ["<" ++ v ++ ">" | v <- vs])
  insts <- case zip3By (\(InstType n _) -> n) fst3 fst3 insts' instImpls instEncs of
    (_, (InstType n _, _):_, _, _, _, _, _) -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding"
    (_, _, (InstType n _, _):_, _, _, _, _) -> throwGlobalError $ "Instruction " ++ n ++ " has no implementation"
    (_, _, _, ((n, vs, _), _):_, _, _, _)   -> throwGlobalError $ "Implementation and encoding given for unknown instruction " ++ instName n vs
    (_, _, _, _, InstType n _:_, _, _)      -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding or implementation"
    (_, _, _, _, _, (n, vs, _):_, _)        -> throwGlobalError $ "Implementation given for unknown instruction " ++ instName n vs
    (_, _, _, _, _, _, (n, vs, _):_)        -> throwGlobalError $ "Encoding given for unknown instruction " ++ instName n vs
    (xs, [], [], [], [], [], [])            -> return [Inst n ts (vs1, rs) (vs2, e) | (InstType n ts, (_, vs1, rs), (_, vs2, e)) <- xs]
  buttons' <- case _rawButtons of
    []  -> throwGlobalError "No register block"
    [x] -> return x
    _   -> throwGlobalError "More than one register block"
  let buttonImpls = mapMaybe (\case ButtonImpl n rs -> Just (n, rs); _ -> Nothing) _rawImpls
  buttons <- case zipBy (\(ButtonType n _) -> n) fst buttons' buttonImpls of
    (_, ButtonType n _:_, _) -> throwGlobalError $ "Button " ++ n ++ " has no implementation"
    (_, _, (n, _):_)         -> throwGlobalError $ "Implementation given for unknown button " ++ n
    (xs, [], [])             -> return [Button n t rs | (ButtonType n t, (_, rs)) <- xs]
  memorys <- case _rawMemorys of
    []  -> throwGlobalError "No register block"
    [x] -> return x
    _   -> throwGlobalError "More than one register block"
  always <- case mapMaybe (\case InstImpl "always" [] rs -> Just rs; _ -> Nothing) _rawImpls of
    []   -> return []
    [rs] -> return rs
    _    -> throwGlobalError "More than one always block"
  leds <- case _rawLedImpls of
    []   -> return []
    [rs] -> return rs
    _    -> throwGlobalError "More than one leds block"
  let encTypes = _rawEncTypes
  return $ Proc{..}
