module WebUSB
  ( Configuration
  , ClosedDevice
  , Device
  , Interface
  , Octet(Octet)
  , USBRecipient(..)
  , USBRequestType(..)
  , octetsToString
  , octetToInt
  , readControl
  , requestDevice
  , usbRead
  , usbWrite
  , withConfiguration
  , withDevice
  , withInterface
  , writeControl
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Except.Trans (ExceptT(ExceptT), lift, runExceptT)
import Control.Promise (Promise, fromAff, toAffE)
import Data.ByteString (fromUTF8, pack)
import Data.Either (Either(Left, Right), either)
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Functor (map) as Functor
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Type.Quotient (mkQuotient)

foreign import data Device :: Type

foreign import _requestDevice :: Int -> Int -> Effect (Promise Device)

foreign import _openDevice :: Device -> Effect (Promise Unit)
foreign import _closeDevice :: Device -> Effect (Promise Unit)

foreign import _selectConfiguration :: Device -> Octet -> Effect (Promise Unit)

foreign import _claimInterface :: Device -> Octet -> Effect (Promise Unit)
foreign import _releaseInterface :: Device -> Octet -> Effect (Promise Unit)

foreign import _controlTransferIn :: Device -> String -> String -> Octet -> Int -> Int -> Int -> Effect (Promise (Array Octet))
foreign import _controlTransferOut :: Device -> String -> String -> Octet -> Int -> Int -> Array Octet -> Effect (Promise Unit)

foreign import _transferIn :: Device -> Octet -> Int -> Effect (Promise (Array Octet))
foreign import _transferOut :: Device -> Octet -> Array Octet -> Effect (Promise Unit)

newtype Octet = Octet Int
derive instance eqOctet :: Eq Octet

instance showOctet :: Show Octet where
  show = toStringAs hexadecimal <<< octetToInt

octetToInt :: Octet -> Int
octetToInt (Octet n) = n

octetsToString :: Array Octet -> String
octetsToString = fromUTF8 <<< pack <<< map (mkQuotient <<< octetToInt)

liftError :: forall e e' m a. MonadError e' m => Show e' => MonadEffect m => e -> m a -> ExceptT e m a
liftError e =
  ExceptT
  <<< flip catchError
    ((<*) (pure <<< Left $ e)
    <<< liftEffect
    <<< log
    <<< show
    )
  <<< Functor.map Right

newtype ClosedDevice = ClosedDevice Device

requestDevice :: forall a. Int -> Int -> (ClosedDevice -> Effect a) -> (String -> Effect a) -> Effect (Promise a)
requestDevice vendorId productId onSuccess onFailure = do
  fromAff
    <<< join
    <<< map (either (liftEffect <<< onFailure) (liftEffect <<< onSuccess))
    <<< runExceptT
    <<< map ClosedDevice
    <<< liftError "USB Error: Could not request device"
    <<< toAffE
    $ _requestDevice vendorId productId

withDevice :: forall a. ClosedDevice -> (Device -> ExceptT String Aff a) -> ExceptT String Aff a
withDevice (ClosedDevice dev) act = do
  liftError "USB Error: Could not open device" <<< toAffE $ _openDevice dev
  res <- act dev
  liftError "USB Error: Could not close device" <<< toAffE $ _closeDevice dev
  pure res

newtype Configuration = Configuration Device

withConfiguration :: forall a. Device -> Octet -> (Configuration -> ExceptT String Aff a) -> ExceptT String Aff a
withConfiguration dev n act = do
  liftError "Could not select configuration" <<< toAffE $ _selectConfiguration dev n
  act <<< Configuration $ dev

newtype Interface = Interface Device

withInterface :: forall a. Configuration -> Octet -> (Interface -> ExceptT String Aff a) -> ExceptT String Aff a
withInterface (Configuration dev) n act = do
  liftError "USB Error: Could not claim interface" <<< toAffE $ _claimInterface dev n
  res <- act <<< Interface $ dev
  liftError "USB Error: Could not release interface" <<< toAffE $ _releaseInterface dev n
  pure res

data USBRequestType
  = Standard
  | Class
  | Vendor

reqTypeToString :: USBRequestType -> String
reqTypeToString Standard = "standard"
reqTypeToString Class    = "class"
reqTypeToString Vendor   = "vendor"

data USBRecipient
  = ToDevice
  | ToInterface
  | ToEndpoint
  | ToOther

recipientToString :: USBRecipient -> String
recipientToString ToDevice    = "device"
recipientToString ToInterface = "interface"
recipientToString ToEndpoint  = "endpoint"
recipientToString ToOther     = "other"

readControl :: Interface -> USBRequestType -> USBRecipient -> Octet -> Int -> Int -> Int -> ExceptT String Aff (Array Octet)
readControl (Interface dev) reqType recipient req val index len =
  (liftError "USB Error: Could not recieve control transfer" <<< toAffE $ _controlTransferIn
    dev
    (reqTypeToString reqType)
    (recipientToString recipient)
    req
    val
    index
    len
    )

writeControl :: Interface -> USBRequestType -> USBRecipient -> Octet -> Int -> Int -> Array Octet -> ExceptT String Aff Unit
writeControl (Interface dev) reqType recipient req val index d =
  liftError "USB Error: Could not send control transfer" <<< toAffE $ _controlTransferOut
    dev
    (reqTypeToString reqType)
    (recipientToString recipient)
    req
    val
    index
    d

usbRead :: Interface -> Octet -> Int -> ExceptT String Aff (Array Octet)
usbRead (Interface dev) endP len =
  liftError "USB Error: Could not read data" <<< toAffE $ _transferIn dev endP len

usbWrite :: Interface -> Octet -> Array Octet -> ExceptT String Aff Unit
usbWrite (Interface dev) endP d =
  liftError "USB Error: Could not write data" <<< toAffE $ _transferOut dev endP d

