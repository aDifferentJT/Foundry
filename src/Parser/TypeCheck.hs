{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Parser.TypeCheck (typeCheck) where

import ClassyPrelude

import Parser.AST
import Parser.Monad

import Utils (intersectionWithKey3)

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

typeCheck :: RawProc -> ParserMonad Proc
typeCheck RawProc{..} = do
  let regEncs = Map.mapMaybe (\case RegEnc e -> Just e; _ -> Nothing) _rawEncs
  let regs = (Map.elems $ Map.intersectionWithKey (\n t e -> Reg n t (Just e)) _rawRegs regEncs)
          ++ (Map.elems . Map.mapWithKey (\n t -> Reg n t Nothing) $ Map.difference _rawRegs regEncs)
  let instEncs = Map.mapMaybe (\case InstEnc vs e -> Just (vs, e); _ -> Nothing) _rawEncs
  let instImpls = Map.mapMaybeWithKey (\n -> \case
        InstImpl vs rs
          | n == "always" -> Nothing
          | otherwise     -> Just (vs, rs)
        _                 -> Nothing
        ) _rawImpls
  let instName n vs = unwords (n : ["<" ++ v ++ ">" | v <- vs])
  {-
  insts <- case zip3By (\(InstType n _) -> n) fst3 fst3 _rawInsts instImpls instEncs of
    (_, (InstType n _, _):_, _, _, _, _, _) -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding"
    (_, _, _, ((n, vs, _), _):_, _, _, _)   -> throwGlobalError $ "Implementation and encoding given for unknown instruction " ++ instName n vs
    (_, _, _, _, InstType n _:_, _, _)      -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding or implementation"
    (_, _, _, _, _, (n, vs, _):_, _)        -> throwGlobalError $ "Implementation given for unknown instruction " ++ instName n vs
    (_, _, _, _, _, _, (n, vs, _):_)        -> throwGlobalError $ "Encoding given for unknown instruction " ++ instName n vs
    (xs, [], [], [], [], [], [])            -> return [Inst n ts (vs1, rs) (vs2, e) | (InstType n ts, (_, vs1, rs), (_, vs2, e)) <- xs]
    -}
  let insts = Map.elems $ intersectionWithKey3 Inst _rawInsts instImpls instEncs
  let buttonImpls = Map.mapMaybe (\case ButtonImpl rs -> Just rs; _ -> Nothing) _rawImpls
  let buttons = Map.elems $ Map.intersectionWithKey Button _rawButtons buttonImpls
  let memorys = _rawMemorys
  let always = concat . Map.elems . Map.mapMaybeWithKey (\n -> \case
        InstImpl [] rs
          | n == "always" -> Just rs
          | otherwise     -> Nothing
        _                 -> Nothing
        ) $ _rawImpls
  let leds = fromMaybe [] _rawLedImpls
  let encTypes = _rawEncTypes
  return $ Proc{..}
