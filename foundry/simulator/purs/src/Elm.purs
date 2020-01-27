module Elm
  ( App
  , init
  , send
  , subscribe
  ) where

import Prelude (Unit)

import Effect (Effect)

foreign import data App :: Type

foreign import init :: Effect App
foreign import send :: forall a. App -> String -> a -> Effect Unit
foreign import subscribe :: forall a. App -> String -> (a -> Effect Unit) -> Effect Unit

