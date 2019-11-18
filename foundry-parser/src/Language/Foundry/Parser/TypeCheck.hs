{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Language.Foundry.Parser.TypeCheck (typeCheck) where

import ClassyPrelude

import Language.Foundry.Parser.AST

import qualified Data.Map.Strict as Map

intersectionWithKey3 :: Ord k => (k -> a -> b -> c -> d) -> Map k a -> Map k b -> Map k c -> Map k d
intersectionWithKey3 f m1 = Map.intersectionWithKey (uncurry . f) . Map.intersectionWith (,) m1

typeCheck :: RawProc -> Proc
typeCheck RawProc{..} = Proc
  { regs =
    let
      regEncs = Map.mapMaybe
        (\case
          RegEnc e -> Just e
          _        -> Nothing
        )
        _rawEncs
    in
    Map.elems (Map.intersectionWithKey Reg _rawRegs (Map.map Just regEncs))
    ++ Map.elems (Map.mapWithKey (\n t -> Reg n t Nothing) $ Map.difference _rawRegs regEncs)
  , insts =
    Map.elems
      ( intersectionWithKey3
        Inst
        _rawInsts
        ( Map.mapMaybeWithKey
          ( curry $ \case
            ("always", _)       -> Nothing
            (_, InstImpl vs rs) -> Just (vs, rs)
            _                   -> Nothing
          )
          _rawImpls
        )
        ( Map.mapMaybe
          (\case
            InstEnc vs e -> Just (vs, e)
            _            -> Nothing
          )
          _rawEncs
        )
      )
  , buttons =
      Map.elems
      . Map.intersectionWithKey Button _rawButtons
      . Map.mapMaybe
        (\case
          ButtonImpl rs -> Just rs
          _             -> Nothing
        )
      $ _rawImpls
  , memorys = _rawMemorys
  , always = concat . Map.elems . Map.mapMaybeWithKey (curry $ \case
      ("always", InstImpl [] rs) -> Just rs
      _                          -> Nothing
    ) $ _rawImpls
  , leds = fromMaybe [] _rawLedImpls
  , encTypes = _rawEncTypes
  }
