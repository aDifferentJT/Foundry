{-# LANGUAGE FlexibleContexts, LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Main(main) where

import ClassyPrelude hiding (Handler)

import Control.Monad.Errors (runErrors, forgive)
import Data.Bit.List (Endianness(Little), bitsToInt)
import GenSimulator (hostSimulatorUpdate)
import Language.Foundry.Parser (parseM, parse')
import Language.Foundry.Parser.AlexPosn (AlexPosn(AlexPosn), Locatable(..))
import Language.Foundry.Parser.Lexer (Token(..), readToken)
import Language.Foundry.Parser.Monad (Defn(..), ParserState(..), initialParserState, runParser')
import Language.Foundry.Proc (Proc)

import Control.Concurrent (forkIO)
import Control.Lens ((^.), view)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.Reader (Reader, runReader)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State (runStateT)
import Data.Default (def)
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Map.Strict as Map
import Data.Rope.UTF16 (toText, splitAtLine)
import Data.SortedList (toSortedList)
import Language.Haskell.LSP.Control
import Language.Haskell.LSP.Core
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as Lens
import Language.Haskell.LSP.VFS
import System.IO (stdin, stdout)

untilM :: Monad m => Int -> (a -> Bool) -> m a -> m (Maybe a)
untilM 0 _ _ = return Nothing
untilM n f m = m >>= \x -> if f x then return . Just $ x else untilM (n-1) f m

data Config = Config
  deriving Show

data ReactorInput
  = SendMessage FromServerMessage
  | ShowDiagnostics Uri
  | CloseDocument
  | ShowHover HoverRequest
  | ShowCompletion CompletionRequest

toRange :: Maybe (AlexPosn, AlexPosn) -> Range
toRange (Just (AlexPosn _ l1 c1, AlexPosn _ l2 c2)) = Range (Position (l1 - 1) (c1 - 1)) (Position (l2 - 1) (c2 - 1))
toRange Nothing                                     = Range (Position 0 0) (Position 0 0)

getTokenInformation :: Token -> Maybe Text
getTokenInformation (Bits (Locatable bs _)) = Just $
  "0b" ++ (mconcat . map tshow $ bs) ++ " = " ++ (tshow . bitsToInt Little $ bs)
getTokenInformation _ = Nothing

reactor :: LspFuncs Config -> TQueue ReactorInput -> (Maybe Proc -> IO ()) -> IO ()
reactor LspFuncs{..} ch updateSimulator = forever . ((liftIO . atomically . readTQueue $ ch) >>=) $ \case
  SendMessage msg -> sendFunc msg
  ShowDiagnostics uri ->
    (getVirtualFileFunc . toNormalizedUri $ uri) >>= \case
      Nothing -> return ()
      Just (VirtualFile version rope _) -> case parse' . toText $ rope of
        Right p -> do
          flushDiagnosticsBySourceFunc 0 . Just $ "Foundry"
          updateSimulator . Just $ p
        Left es -> do
          let ds = Map.fromList
                [ ( Just "Foundry"
                  , toSortedList
                    [ Diagnostic
                        (toRange range)
                        (Just DsError)
                        Nothing
                        (Just "Foundry")
                        e
                        Nothing
                      | (e, range) <- es
                    ]
                  )
                ]
          publishDiagnosticsFunc 100 (toNormalizedUri uri) (Just version) ds
          updateSimulator Nothing
  CloseDocument -> updateSimulator Nothing
  ShowHover req -> void . runMaybeT $ do
    VirtualFile _ rope _ <- MaybeT . getVirtualFileFunc . toNormalizedUri $ req ^. Lens.params . Lens.textDocument . Lens.uri
    let Position lineNum col = req ^. Lens.params . Lens.position
    let line = toText . fst . splitAtLine 1 . snd . splitAtLine lineNum $ rope
    tok <- MaybeT
      . return
      . join
      . rightToMaybe
      . runParser'
        ( untilM
            (length line)
            (\(Locatable _ (Just (AlexPosn _ _ c1, AlexPosn _ _ c2))) -> c1 <= col + 1 && col + 1 < c2)
            readToken
          )
      $ line
    info <- MaybeT
      . return
      . getTokenInformation
      . locatableValue
      $ tok
    lift
      . sendFunc
      . RspHover
      . makeResponseMessage req
      . Just
      . flip Hover Nothing
      . HoverContents
      . MarkupContent MkPlainText
      $ info
  ShowCompletion req -> void . runMaybeT $ do
    VirtualFile _ rope _ <- MaybeT . getVirtualFileFunc . toNormalizedUri $ req ^. Lens.params . Lens.textDocument . Lens.uri
    ParserState{..} <- MaybeT . return . rightToMaybe . runErrors . forgive . (snd <$>) . runStateT parseM . initialParserState . toText . fst . splitAtLine (req ^. Lens.params . Lens.position . Lens.line + 1) $ rope
    --ParserState{..} <- MaybeT . return . rightToMaybe . runErrors . forgive . (snd <$>) . runStateT parseM . initialParserState . toText $ rope
    lift
      . sendFunc
      . RspCompletion
      . makeResponseMessage req
      . Completions
      . List
      $ [ CompletionItem 
            var
            (Just CiVariable)
            (Just $ "Of type " ++ tshow t)
            Nothing
            (Just False)
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
        | (var, t) <- Map.toList stateLocalVars
        ]
      ++ [ CompletionItem 
             var
             (Just CiVariable)
             ( Just $ case locatableValue defn of
                 RegDefn  n     -> "A register of width " ++ tshow n
                 InstDefn _     -> "An instruction"
                 ButtonDefn     -> "A button" ++ tshow (req ^. Lens.params . Lens.position . Lens.line)
                 MemoryDefn a d -> "A memory with address width " ++ tshow a ++ " and data width " ++ tshow d
               )
             Nothing
             (Just False)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
           | (var, defn) <- Map.toList stateGlobalIdents
         ]
    return ()

type HandlerMonad a = Reader (TQueue ReactorInput) (Handler a)

sendToReactor :: (a -> ReactorInput) -> HandlerMonad a
sendToReactor f = (\ch -> atomically . writeTQueue ch . f) <$> Reader.ask

-- InitializeCallbacks
onInitialConfiguration :: InitializeRequest -> Either Text Config
onInitialConfiguration _ = Right Config

onConfigurationChange :: DidChangeConfigurationNotification -> Either Text Config
onConfigurationChange _ = Right Config

onStartup :: TQueue ReactorInput -> LspFuncs Config -> IO (Maybe ResponseError)
onStartup ch funcs = do
  updateSimulator <- hostSimulatorUpdate 8000
  _ <- forkIO $ reactor funcs ch updateSimulator
  return Nothing

-- Handlers
hoverHandler                             :: HandlerMonad HoverRequest
hoverHandler                             = sendToReactor ShowHover

completionHandler                        :: HandlerMonad CompletionRequest
completionHandler                        = sendToReactor ShowCompletion

reloadErrorsHandler :: (Lens.HasParams n p, Lens.HasTextDocument p d, Lens.HasUri d Uri) => HandlerMonad n
reloadErrorsHandler =
  sendToReactor
    ( ShowDiagnostics
    . view (Lens.params . Lens.textDocument . Lens.uri)
    )

didOpenTextDocumentNotificationHandler   :: HandlerMonad DidOpenTextDocumentNotification
didOpenTextDocumentNotificationHandler   = reloadErrorsHandler

didChangeTextDocumentNotificationHandler :: HandlerMonad DidChangeTextDocumentNotification
didChangeTextDocumentNotificationHandler = reloadErrorsHandler

didCloseTextDocumentNotificationHandler  :: HandlerMonad DidCloseTextDocumentNotification
didCloseTextDocumentNotificationHandler  = sendToReactor (const CloseDocument)

didSaveTextDocumentNotificationHandler   :: HandlerMonad DidSaveTextDocumentNotification
didSaveTextDocumentNotificationHandler   = reloadErrorsHandler

-- Options
textDocumentSync :: TextDocumentSyncOptions
textDocumentSync =
  TextDocumentSyncOptions
    (Just True)
    (Just TdSyncIncremental)
    (Just False)
    (Just False)
    (Just (SaveOptions (Just False)))


-- Main
main :: IO ()
main = do
  ch <- atomically newTQueue
  _ <- runWithHandles
    stdin
    stdout
    InitializeCallbacks
      { onInitialConfiguration = Main.onInitialConfiguration
      , onConfigurationChange  = Main.onConfigurationChange
      , onStartup              = Main.onStartup ch
      }
    def
      { LSP.hoverHandler                             = Just . runReader Main.hoverHandler $ ch
      , LSP.completionHandler                        = Just . runReader Main.completionHandler $ ch
      , LSP.didOpenTextDocumentNotificationHandler   = Just . runReader Main.didOpenTextDocumentNotificationHandler $ ch
      , LSP.didChangeTextDocumentNotificationHandler = Just . runReader Main.didChangeTextDocumentNotificationHandler $ ch
      , LSP.didCloseTextDocumentNotificationHandler  = Just . runReader Main.didCloseTextDocumentNotificationHandler $ ch
      , LSP.didSaveTextDocumentNotificationHandler   = Just . runReader Main.didSaveTextDocumentNotificationHandler $ ch
      }
    def
      { LSP.textDocumentSync = Just Main.textDocumentSync
      }
    (Just "lang_server.log")
  return ()

