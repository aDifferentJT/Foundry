module Main where

import Prelude

import Elm (init, subscribe) as Elm

import IceBurn (Octet)

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  app <- Elm.init
  Elm.subscribe app "burn" $ \(bs :: Array Octet) -> do
    log "Burning:"
    log (show bs)
  log "Hello sailor!"
