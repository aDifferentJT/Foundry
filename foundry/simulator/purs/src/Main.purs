module Main where

import Prelude

import ElmPorts (init, send, sendNull, subscribe) as Elm

import IceBurn (Octet, burn)

import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(Left, Right))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)

main :: Effect Unit
main = do
  app <- Elm.init
  Elm.subscribe app "burn" $
    runAff_
      (case _ of
        Left e -> do
          log (show e)
          Elm.send app "burnFinished" "JS Error (details in console)"
        Right (Left e)     -> Elm.send app "burnFinished" e
        Right (Right unit) -> Elm.sendNull app "burnFinished"
      )
    <<< runExceptT
    <<< burn

