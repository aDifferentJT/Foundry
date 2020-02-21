module Main (burn) where

import Prelude

import Ice40Board

import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Array (cons, drop, head, length, take, zip, (..))
import Data.Either (Either(Left, Right))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n xs =
  if length xs <= n
  then [xs]
  else cons (take n xs) (chunksOf n (drop n xs))

burn :: (String -> Effect Unit) -> Effect Unit -> Array Octet -> Effect Unit
burn onError onSuccess bs =
  runAff_
    (case _ of
      Left e -> do
        log (show e)
        onError "JS Error (details in console)"
      Right (Left e)     -> onError e
      Right (Right unit) -> onSuccess
    )
  <<< runExceptT
  <<< withBoard $ \board ->
    boardWithGpio board $ \gpio -> do
      gpioSetReset gpio true
      boardWithSpi board (Octet 0) $ \spi -> do
        _ <- spiSetSpeed spi 50_000_000
        spiSetMode spi

        let flash = M25P10Flash <<< spiIoFunc $ spi
        flashWakeup flash
        flashId <- flashGetId flash
        unless (flashId == [Octet 0x20, Octet 0x20, Octet 0x11]) <<< throwError
          $ "ID incorrect, this may not be the correct flash, it has ID " <> (show flashId)

        -- Erase
        flashChipErase flash

        -- Write
        _ <- traverse (\xs -> flashPageProgram flash (maybe 0 fst <<< head $ xs) <<< map snd $ xs) <<< chunksOf 256 <<< zip (0 .. length bs) $ bs

        -- Verify
        buf <- flashRead flash 0 (length bs)
        unless (buf == bs) <<< throwError $ "Image doesn't match"

      gpioSetReset gpio false

