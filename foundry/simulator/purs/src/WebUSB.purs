module WebUSB
  ( Configuration
  , Device
  , Interface
  , Octet(Octet)
  , USBRecipient(..)
  , USBRequestType(..)
  , octetsToString
  , octetToInt
  , readControl
  , usbRead
  , usbWrite
  , withDevice
  , withConfiguration
  , withInterface
  , writeControl
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT, lift, throwError)
import Control.Promise (Promise, toAffE)
import Data.ByteString (fromUTF8, pack)
import Data.Function.Uncurried (Fn2, Fn3, Fn5, Fn7, Fn9, runFn2, runFn3, runFn5, runFn7, runFn9)
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Type.Quotient (mkQuotient)

foreign import data Device :: Type

foreign import _requestDevice :: Fn2 Int Int (Effect (Promise Device))
foreign import _openDevice :: Device -> Effect (Promise Unit)
foreign import _closeDevice :: Device -> Effect (Promise Unit)
foreign import _selectConfiguration :: Fn2 Device Octet (Effect (Promise Unit))
foreign import _claimInterface :: Fn2 Device Octet (Effect (Promise Unit))
foreign import _releaseInterface :: Fn2 Device Octet (Effect (Promise Unit))
foreign import _controlTransferIn :: Fn9 (Maybe (Array Octet)) (Array Octet -> Maybe (Array Octet)) Device String String Octet Int Int Int (Effect (Promise (Maybe (Array Octet))))
foreign import _controlTransferOut :: Fn7 Device String String Octet Int Int (Array Octet) (Effect (Promise Unit))
foreign import _transferIn :: Fn5 (Maybe (Array Octet)) (Array Octet -> Maybe (Array Octet)) Device Octet Int (Effect (Promise (Maybe (Array Octet))))
foreign import _transferOut :: Fn3 Device Octet (Array Octet) (Effect (Promise Unit))

newtype Octet = Octet Int
derive instance eqOctet :: Eq Octet

instance showOctet :: Show Octet where
  show = toStringAs hexadecimal <<< octetToInt

octetToInt :: Octet -> Int
octetToInt (Octet n) = n

octetsToString :: Array Octet -> String
octetsToString = fromUTF8 <<< pack <<< map (mkQuotient <<< octetToInt)

withDevice :: forall a. Int -> Int -> (Device -> ExceptT String Aff a) -> ExceptT String Aff a
withDevice vendorId productId act = do
  dev <- lift <<< toAffE $ runFn2 _requestDevice vendorId productId
  lift <<< toAffE $ _openDevice dev
  res <- act dev
  lift <<< toAffE $ _closeDevice dev
  pure res

newtype Configuration = Configuration Device

withConfiguration :: forall a. Device -> Octet -> (Configuration -> ExceptT String Aff a) -> ExceptT String Aff a
withConfiguration dev n act = do
  lift <<< toAffE $ runFn2 _selectConfiguration dev n
  act <<< Configuration $ dev

newtype Interface = Interface Device

withInterface :: forall a. Configuration -> Octet -> (Interface -> ExceptT String Aff a) -> ExceptT String Aff a
withInterface (Configuration dev) n act = do
  lift <<< toAffE $ runFn2 _claimInterface dev n
  res <- act <<< Interface $ dev
  lift <<< toAffE $ runFn2 _releaseInterface dev n
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
  (lift <<< toAffE $ runFn9
    _controlTransferIn
    Nothing
    Just
    dev
    (reqTypeToString reqType)
    (recipientToString recipient)
    req
    val
    index
    len
    )
  >>= maybe (throwError "Control Read Failed") pure

writeControl :: Interface -> USBRequestType -> USBRecipient -> Octet -> Int -> Int -> Array Octet -> ExceptT String Aff Unit
writeControl (Interface dev) reqType recipient req val index d =
  lift <<< toAffE $ runFn7
    _controlTransferOut
    dev
    (reqTypeToString reqType)
    (recipientToString recipient)
    req
    val
    index
    d

usbRead :: Interface -> Octet -> Int -> ExceptT String Aff (Array Octet)
usbRead (Interface dev) endP len =
  (lift <<< toAffE $ runFn5 _transferIn Nothing Just dev endP len)
  >>= maybe (throwError "Read Failed") pure

usbWrite :: Interface -> Octet -> Array Octet -> ExceptT String Aff Unit
usbWrite (Interface dev) endP d =
  lift <<< toAffE $ runFn3 _transferOut dev endP d

