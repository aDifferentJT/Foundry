{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Parser.TypeCheck (typeCheck) where

import ClassyPrelude

import Parser.AST
import Parser.Monad

import Utils (fst3, zipBy, zip3By)

import Data.Maybe (mapMaybe)

typeCheck :: RawProc -> ParserMonad Proc
typeCheck RawProc{..} = do
  let regs' = fromMaybe [] _rawRegs
  let regEncs = mapMaybe (\case RegEnc n e -> Just (n, e); _ -> Nothing) _rawEncs
  let regs =
        let (xs, ys, []) = zipBy (\(RegType n _) -> n) fst regs' regEncs in
        ([Reg n t (Just e)  | (RegType n t, (_, e)) <- xs] ++ [Reg n t Nothing | (RegType n t) <- ys])
  let insts' = fromMaybe [] _rawInsts
  let instEncs = mapMaybe (\case InstEnc n vs e -> Just (n, vs, e); _ -> Nothing) _rawEncs
  let instImpls = mapMaybe (\case InstImpl "always" _ _ -> Nothing; InstImpl n vs rs -> Just (n, vs, rs); _ -> Nothing) _rawImpls
  let instName n vs = unwords (n : ["<" ++ v ++ ">" | v <- vs])
  insts <- case zip3By (\(InstType n _) -> n) fst3 fst3 insts' instImpls instEncs of
    (_, (InstType n _, _):_, _, _, _, _, _) -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding"
    (_, _, _, ((n, vs, _), _):_, _, _, _)   -> throwGlobalError $ "Implementation and encoding given for unknown instruction " ++ instName n vs
    (_, _, _, _, InstType n _:_, _, _)      -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding or implementation"
    (_, _, _, _, _, (n, vs, _):_, _)        -> throwGlobalError $ "Implementation given for unknown instruction " ++ instName n vs
    (_, _, _, _, _, _, (n, vs, _):_)        -> throwGlobalError $ "Encoding given for unknown instruction " ++ instName n vs
    (xs, [], [], [], [], [], [])            -> return [Inst n ts (vs1, rs) (vs2, e) | (InstType n ts, (_, vs1, rs), (_, vs2, e)) <- xs]
  let buttons' = fromMaybe [] _rawButtons
  let buttonImpls = mapMaybe (\case ButtonImpl n rs -> Just (n, rs); _ -> Nothing) _rawImpls
  let buttons =
        let (xs, [], []) = zipBy (\(ButtonType n _) -> n) fst buttons' buttonImpls in
        [Button n t rs | (ButtonType n t, (_, rs)) <- xs]
  let memorys = fromMaybe [] _rawMemorys
  let always = concat . mapMaybe (\case InstImpl "always" [] rs -> Just rs; _ -> Nothing) $ _rawImpls
  let leds = fromMaybe [] _rawLedImpls
  let encTypes = _rawEncTypes
  return $ Proc{..}
