{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude #-}

{-|
Module      : Data.NonNull.Append
Description : Append mono to NonNull mono
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Data.NonNull.Append
  ( appendL
  , appendR
  ) where

import ClassyPrelude

appendL :: IsSequence mono => mono -> NonNull mono -> NonNull mono
appendL xs ys = case uncons xs of
  Nothing      -> ys
  Just (x,xs') -> ncons x $ xs' ++ toNullable ys

appendR :: IsSequence mono => NonNull mono -> mono -> NonNull mono
appendR xs ys =
  let (x, xs') = splitFirst xs in
  ncons x $ xs' <> ys

