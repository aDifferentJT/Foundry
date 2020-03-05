module Ice40Board
  ( module WebUSBReexport
  , requestDevice
  , Ice40Board
  , withBoard
  , boardGetSerial
  , GPIO
  , boardWithGpio
  , gpioSetReset
  , SPI
  , boardWithSpi
  , spiSetSpeed
  , spiSetMode
  , spiIoFunc
  , M25P10Flash(..)
  , flashWakeup
  , flashSetWritable
  , flashChipErase
  , flashRead
  , flashPageProgram
  , flashWaitDone
  , flashGetStatus
  , flashGetId
  ) where

import WebUSB hiding (requestDevice)
import WebUSB as WebUSB
import WebUSB (ClosedDevice, Octet(..)) as WebUSBReexport

import Prelude

import Control.Monad.Except (ExceptT, class MonadError, catchError, throwError)
import Control.Promise (Promise)
import Data.Array (concat, cons, drop, head, index, length, replicate, take, takeWhile, tail, zip)
import Data.Either (Either(Left, Right))
import Data.Int.Bits (shl, shr, (.|.), (.&.))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Unfoldable (unfoldr)
import Effect (Effect)
import Effect.Aff (Aff)

import Effect.Class (liftEffect)
import Effect.Console (log)

zipMaybe :: forall a b. Array a -> Array b -> Array (Tuple (Maybe a) (Maybe b))
zipMaybe xs ys =
  let n = length xs - length ys in
  if n > 0
  then zip (map Just xs) (map Just ys <> replicate n Nothing)
  else zip (map Just xs <> replicate (-n) Nothing) (map Just ys)

wrapError :: forall e m a. MonadError e m => m Unit -> m a -> m Unit -> m a
wrapError pre act post = do
  pre
  res <- catchError (Left <$> act) (pure <<< Right)
  post
  case res of
    Left x  -> pure x
    Right e -> throwError e

tupleMap :: forall a b c d. (a -> c) -> (b -> d) -> Tuple a b -> Tuple c d
tupleMap f g (Tuple x y) = Tuple (f x) (g y)

infixr 5 tupleMap as ***

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n xs =
  if length xs <= n
  then [xs]
  else cons (take n xs) (chunksOf n (drop n xs))

data Endianness
  = Little
  | Big

encodeWidth :: Endianness -> Int -> Int -> Array Octet
encodeWidth _ 1 x = [Octet x]
encodeWidth e w x = case e of
    Little -> ls <> bs
    Big    -> bs <> ls
  where ls = encodeWidth e (w / 2) x
        bs = encodeWidth e (w - (w / 2)) (shr x (8 * (w / 2)))

decodeWidth :: Endianness -> Int -> Array Octet -> Int
decodeWidth _ 1 bs = maybe 0 octetToInt <<< head $ bs
decodeWidth e w bs = case e of
    Little -> x .|. shl y (8 * (w / 2))
    Big    -> shl x (8 * (w / 2)) .|. y
  where x = decodeWidth e (w / 2) <<< take (w / 2) $ bs
        y = decodeWidth e (w - (w / 2)) <<< drop (w / 2) $ bs

ice40VendorId :: Int
ice40VendorId = 0x1443

ice40ProductId :: Int
ice40ProductId = 0x0007

cmdOut :: Octet
cmdOut = Octet 1

cmdIn :: Octet
cmdIn = Octet 2

dataOut :: Octet
dataOut = Octet 3

dataIn :: Octet
dataIn = Octet 4

newtype Ice40Board = Ice40Board Interface

requestDevice :: forall a. (ClosedDevice -> Effect a) -> (String -> Effect a) -> Effect (Promise a)
requestDevice = WebUSB.requestDevice ice40VendorId ice40ProductId

withBoard :: forall a. ClosedDevice -> (Ice40Board -> ExceptT String Aff a) -> ExceptT String Aff a
withBoard closedDev act =
  withDevice closedDev $ \dev ->
    withConfiguration dev (Octet 1) $ \conf ->
      withInterface conf (Octet 0) $ \intf -> do
        let board = Ice40Board intf
        bType <- boardGetType board
        unless (bType == "iCE40") <<< throwError $ "Board type " <> bType <> " does not match expected \"iCE40\""
        act board

boardGetType :: Ice40Board -> ExceptT String Aff String
boardGetType board =
  octetsToString <<< takeWhile ((/=) (Octet 0x00)) <$> boardCtrlRead board (Octet 0xE2) 16

boardGetSerial :: Ice40Board -> ExceptT String Aff String
boardGetSerial board =
  octetsToString <<< takeWhile ((/=) (Octet 0x00)) <$> boardCtrlRead board (Octet 0xE2) 16

boardCtrlRead :: Ice40Board -> Octet -> Int -> ExceptT String Aff (Array Octet)
boardCtrlRead (Ice40Board intf) req size =
  readControl intf Vendor ToDevice req 0 0 size

boardCmdI :: Ice40Board -> Array Octet -> Int -> ExceptT String Aff (Array Octet)
boardCmdI (Ice40Board intf) bs size = do
  usbWrite intf cmdOut (cons (Octet <<< length $ bs) bs)
  usbRead intf cmdIn size

boardCmd :: Ice40Board -> Octet -> Octet -> Array Octet -> Int -> ExceptT String Aff (Array Octet)
boardCmd board cmd subCmd =
  boardCmdI board <<< cons cmd <<< cons subCmd

newtype GPIO = GPIO Ice40Board

boardWithGpio :: forall a. Ice40Board -> (GPIO -> ExceptT String Aff a) -> ExceptT String Aff a
boardWithGpio board act = wrapError
  (void $ boardCmd board (Octet 0x03) (Octet 0x00) [Octet 0x00] 16)
  (act <<< GPIO $ board)
  (void $ boardCmd board (Octet 0x03) (Octet 0x01) [Octet 0x00] 16)

gpioSetDir :: GPIO -> Octet -> ExceptT String Aff Unit
gpioSetDir (GPIO board) dir = void $ boardCmd
  board
  (Octet 0x03)
  (Octet 0x04)
  [ Octet 0x00
  , dir
  , Octet 0x00
  , Octet 0x00
  , Octet 0x00
  ]
  16

gpioSetValue :: GPIO -> Octet -> ExceptT String Aff Unit
gpioSetValue (GPIO board) value = void $ boardCmd
  board
  (Octet 0x03)
  (Octet 0x06)
  [ Octet 0x00
  , value
  , Octet 0x00
  , Octet 0x00
  , Octet 0x00
  ]
  16

gpioSetReset :: GPIO -> Boolean -> ExceptT String Aff Unit
gpioSetReset gpio true  = gpioSetDir gpio (Octet 1) *> gpioSetValue gpio (Octet 0)
gpioSetReset gpio false = gpioSetDir gpio (Octet 0)

newtype SPI = SPI Ice40Board

boardWithSpi :: forall a. Ice40Board -> Octet -> (SPI -> ExceptT String Aff a) -> ExceptT String Aff a
boardWithSpi board portNum act = wrapError
  (void $ boardCmd board (Octet 0x06) (Octet 0x00) [portNum] 16)
  (act <<< SPI $ board)
  (void $ boardCmd board (Octet 0x06) (Octet 0x01) [portNum] 16)

spiSetSpeed :: SPI -> Int -> ExceptT String Aff Int -- n.b. Speed is 32 bits
spiSetSpeed (SPI board) speed =
  decodeWidth Little 4 <$> boardCmd board (Octet 0x06) (Octet 0x03) (cons (Octet 0x00) (encodeWidth Little 4 speed)) 16

spiSetMode :: SPI -> ExceptT String Aff Unit
spiSetMode (SPI board) =
  void $ boardCmd board (Octet 0x06) (Octet 0x05) [Octet 0x00, Octet 0x00] 16

spiIoFunc :: SPI -> Array Octet -> Int -> ExceptT String Aff (Array Octet)
spiIoFunc (SPI board@(Ice40Board dev)) writeBytes readSize = do
  let writeBytes' = writeBytes <> replicate (readSize - length writeBytes) (Octet 0)
  _ <- boardCmd board (Octet 0x06) (Octet 0x06) [Octet 0x00, Octet 0x00] 16 -- SPI Start
  _ <- boardCmd
    board
    (Octet 0x06)
    (Octet 0x07)
    ( [Octet 0x00, Octet 0x00, Octet 0x00, if readSize > 0 then Octet 0x01 else Octet 0x00]
    <> (encodeWidth Little 4 <<< length $ writeBytes')
    )
    16 -- SPI Aff Start
  let writeChunks = chunksOf 64 writeBytes'
  let readSizes = unfoldr
        ( case _ of
          0 -> Nothing
          x -> let y = min 64 x in Just (Tuple y (x - y))
        )
        readSize
  readBytes <- concat <$> traverse
    (uncurry (*>) <<< (maybe (pure unit) (usbWrite dev dataOut) *** maybe (pure []) (usbRead dev dataIn)))
    (zipMaybe writeChunks readSizes)
  _ <- boardCmd board (Octet 0x06) (Octet 0x87) [Octet 0x00] 16
  _ <- boardCmd board (Octet 0x06) (Octet 0x06) [Octet 0x00, Octet 0x01] 16
  pure readBytes

newtype M25P10Flash = M25P10Flash (Array Octet -> Int -> ExceptT String Aff (Array Octet))

flashStatBusy :: Octet
flashStatBusy = Octet 0x1

flashCmdGetStatus :: Octet
flashCmdGetStatus = Octet 0x05
flashCmdWriteEnable :: Octet
flashCmdWriteEnable = Octet 0x6
flashCmdReadId :: Octet
flashCmdReadId = Octet 0x9F
flashCmdWakeUp :: Octet
flashCmdWakeUp = Octet 0xAB
flashCmdChipErase :: Octet
flashCmdChipErase = Octet 0xC7
flashCmdPageProgram :: Octet
flashCmdPageProgram = Octet 0x02
flashCmdFastRead :: Octet
flashCmdFastRead = Octet 0xB

flashWakeup :: M25P10Flash -> ExceptT String Aff Unit
flashWakeup (M25P10Flash io) = void $ io [flashCmdWakeUp] 0

flashSetWritable :: M25P10Flash -> ExceptT String Aff Unit
flashSetWritable (M25P10Flash io) = void $ io [flashCmdWriteEnable] 0

flashChipErase :: M25P10Flash -> ExceptT String Aff Unit
flashChipErase flash@(M25P10Flash io) = do
  flashSetWritable flash
  void $ io [flashCmdChipErase] 0
  flashWaitDone flash

flashRead :: M25P10Flash -> Int -> Int -> ExceptT String Aff (Array Octet) -- n.b. addr is 32 bits
flashRead (M25P10Flash io) addr size =
  drop 5 <$> io 
    [ flashCmdFastRead
    , Octet (shr addr 16)
    , Octet (shr addr 8)
    , Octet addr
    , Octet 0x00
    ]
    ( size + 5 )

flashPageProgram :: M25P10Flash -> Int -> Array Octet -> ExceptT String Aff Unit -- n.b. addr is 32 bits
flashPageProgram flash@(M25P10Flash io) addr buf = do
  flashSetWritable flash
  _ <- io
    ( [ flashCmdPageProgram
      , Octet (shr addr 16)
      , Octet (shr addr 8)
      , Octet addr
      ] <> buf
      )
    0
  flashWaitDone flash

flashWaitDone :: M25P10Flash -> ExceptT String Aff Unit
flashWaitDone flash = ((.&.) (octetToInt flashStatBusy) <<< octetToInt) <$> flashGetStatus flash >>= case _ of
  0 -> pure unit
  _ -> flashWaitDone flash

flashGetStatus :: M25P10Flash -> ExceptT String Aff Octet
flashGetStatus (M25P10Flash io) =
  io [flashCmdGetStatus] 2
  >>= maybe (throwError "Failed to get flash status") pure <<< flip index 1

flashGetId :: M25P10Flash -> ExceptT String Aff (Array Octet)
flashGetId (M25P10Flash io) =
  io [flashCmdReadId] 4 >>= tail >>> maybe (throwError "Failed to get flash id") pure

