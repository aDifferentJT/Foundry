{-# LANGUAGE BlockArguments, LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Main(main) where

import ClassyPrelude hiding (Handler)

import Parser (parse')
import Parser.AlexPosn (AlexPosn(AlexPosn))

import Data.Maybe (fromMaybe, listToMaybe)

import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad (when, unless, forever, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Either.Combinators (leftToMaybe)
import qualified Data.Map as Map
import Data.Rope.UTF16 (toText)
import Data.SortedList (toSortedList)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.LSP.Control
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Utility
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import Language.Haskell.LSP.VFS
import System.IO (stdin, stdout)

data Config = Config
  deriving Show

data ReactorInput
  = SendMessage FromServerMessage
  | ShowDiagnostics Uri

toRange :: Maybe (AlexPosn, AlexPosn) -> Range
toRange (Just (AlexPosn _ l1 c1, AlexPosn _ l2 c2)) = Range (Position (l1 - 1) (c1 - 1)) (Position (l2 - 1) (c2 - 1))

reactor :: LspFuncs Config -> TMChan ReactorInput -> IO ()
reactor LspFuncs{..} ch = forever . ((liftIO . atomically . readTMChan $ ch) >>=) $ \case
  Just (SendMessage msg) -> sendFunc msg
  Just (ShowDiagnostics uri) -> void . runMaybeT $ do
    VirtualFile version rope _ <- MaybeT . getVirtualFileFunc . toNormalizedUri $ uri
    case parse' . toText $ rope of
      Right _ -> lift . flushDiagnosticsBySourceFunc 0 . Just $ "Foundry"
      Left es ->
        let ds = Map.fromList
              [ ( Just "Foundry"
                , toSortedList
                  [ Diagnostic
                      (toRange range)
                      (Just DsError)
                      Nothing
                      (Just "Foundry")
                      error
                      Nothing
                    | (error, range) <- es
                  ]
                )
              ] in
        lift . publishDiagnosticsFunc 100 (toNormalizedUri uri) (Just version) $ ds
  Nothing -> return ()

-- InitializeCallbacks
onInitialConfiguration :: InitializeRequest -> Either Text Config
onInitialConfiguration m = Right Config

onConfigurationChange :: DidChangeConfigurationNotification -> Either Text Config
onConfigurationChange _ = Right Config

onStartup :: TMChan ReactorInput -> LspFuncs Config -> IO (Maybe ResponseError)
onStartup ch funcs = do
  forkIO $ reactor funcs ch
  return Nothing

-- Handlers
hoverHandler                                 :: TMChan ReactorInput -> Handler HoverRequest
hoverHandler ch req                          = do
  atomically
  . writeTMChan ch
  . SendMessage 
  . RspHover
  . makeResponseMessage req
  . Just
  $ Hover (HoverContents (MarkupContent MkPlainText "Hover text")) Nothing

completionHandler                            :: Handler CompletionRequest
completionHandler req                        = do
  let ps = req ^. params
  return ()

completionResolveHandler                     :: Handler CompletionItemResolveRequest
completionResolveHandler req                 = do
  return ()

signatureHelpHandler                         :: Maybe (Handler SignatureHelpRequest)
signatureHelpHandler                         = Nothing

definitionHandler                            :: Maybe (Handler DefinitionRequest)
definitionHandler                            = Just (const (return ()))

typeDefinitionHandler                        :: Maybe (Handler TypeDefinitionRequest)
typeDefinitionHandler                        = Nothing

implementationHandler                        :: Maybe (Handler ImplementationRequest)
implementationHandler                        = Nothing

referencesHandler                            :: Maybe (Handler ReferencesRequest)
referencesHandler                            = Nothing

documentHighlightHandler                     :: Maybe (Handler DocumentHighlightRequest)
documentHighlightHandler                     = Nothing

documentSymbolHandler                        :: Maybe (Handler DocumentSymbolRequest)
documentSymbolHandler                        = Nothing

workspaceSymbolHandler                       :: Maybe (Handler WorkspaceSymbolRequest)
workspaceSymbolHandler                       = Nothing

codeActionHandler                            :: Maybe (Handler CodeActionRequest)
codeActionHandler                            = Nothing

codeLensHandler                              :: Maybe (Handler CodeLensRequest)
codeLensHandler                              = Nothing

codeLensResolveHandler                       :: Maybe (Handler CodeLensResolveRequest)
codeLensResolveHandler                       = Nothing

documentColorHandler                         :: Maybe (Handler DocumentColorRequest)
documentColorHandler                         = Nothing

colorPresentationHandler                     :: Maybe (Handler ColorPresentationRequest)
colorPresentationHandler                     = Nothing

documentFormattingHandler                    :: Maybe (Handler DocumentFormattingRequest)
documentFormattingHandler                    = Nothing

documentRangeFormattingHandler               :: Maybe (Handler DocumentRangeFormattingRequest)
documentRangeFormattingHandler               = Nothing

documentTypeFormattingHandler                :: Maybe (Handler DocumentOnTypeFormattingRequest)
documentTypeFormattingHandler                = Nothing

renameHandler                                :: Handler RenameRequest
renameHandler req                            = do
  return ()

prepareRenameHandler                         :: Maybe (Handler PrepareRenameRequest)
prepareRenameHandler                         = Nothing

foldingRangeHandler                          :: Maybe (Handler FoldingRangeRequest)
foldingRangeHandler                          = Nothing

documentLinkHandler                          :: Maybe (Handler DocumentLinkRequest)
documentLinkHandler                          = Nothing

documentLinkResolveHandler                   :: Maybe (Handler DocumentLinkResolveRequest)
documentLinkResolveHandler                   = Nothing

executeCommandHandler                        :: Maybe (Handler ExecuteCommandRequest)
executeCommandHandler                        = Nothing

willSaveWaitUntilTextDocHandler              :: Maybe (Handler WillSaveWaitUntilTextDocumentRequest)
willSaveWaitUntilTextDocHandler              = Nothing

didChangeConfigurationParamsHandler          :: Maybe (Handler DidChangeConfigurationNotification)
didChangeConfigurationParamsHandler          = Nothing

didOpenTextDocumentNotificationHandler       :: TMChan ReactorInput -> Handler DidOpenTextDocumentNotification
didOpenTextDocumentNotificationHandler ch notif =
  atomically
  . writeTMChan ch
  . ShowDiagnostics
  $ notif ^. params . textDocument . uri

didChangeTextDocumentNotificationHandler     :: TMChan ReactorInput -> Handler DidChangeTextDocumentNotification
didChangeTextDocumentNotificationHandler ch notif =
  atomically
  . writeTMChan ch
  . ShowDiagnostics
  $ notif ^. params . textDocument . uri

didCloseTextDocumentNotificationHandler      :: TMChan ReactorInput -> Handler DidCloseTextDocumentNotification
didCloseTextDocumentNotificationHandler ch notif = return ()

didSaveTextDocumentNotificationHandler       :: TMChan ReactorInput -> Handler DidSaveTextDocumentNotification
didSaveTextDocumentNotificationHandler ch notif =
  atomically
  . writeTMChan ch
  . ShowDiagnostics
  $ notif ^. params . textDocument . uri

didChangeWatchedFilesNotificationHandler     :: Maybe (Handler DidChangeWatchedFilesNotification)
didChangeWatchedFilesNotificationHandler     = Nothing

didChangeWorkspaceFoldersNotificationHandler :: Maybe (Handler DidChangeWorkspaceFoldersNotification)
didChangeWorkspaceFoldersNotificationHandler = Nothing

initializedHandler                           :: Handler InitializedNotification
initializedHandler notif                     =
  logs $ "Received init: " ++ show notif

willSaveTextDocumentNotificationHandler      :: Maybe (Handler WillSaveTextDocumentNotification)
willSaveTextDocumentNotificationHandler      = Nothing

cancelNotificationHandler                    :: Handler CancelNotification
cancelNotificationHandler notif              = do
  return ()

responseHandler                              :: Handler BareResponseMessage
responseHandler resp                         =
  logs $ "Received response: " ++ show resp

initializeRequestHandler                     :: Handler InitializeRequest
initializeRequestHandler req                 =
  logs $ "Received init: " ++ show req

exitNotificationHandler                      :: Maybe (Handler ExitNotification)
exitNotificationHandler                      = Nothing

customRequestHandler                         :: Maybe (Handler CustomClientRequest)
customRequestHandler                         = Nothing

customNotificationHandler                    :: Maybe (Handler CustomClientNotification)
customNotificationHandler                    = Nothing

-- Options
textDocumentSync                 :: TextDocumentSyncOptions
textDocumentSync                 = TextDocumentSyncOptions (Just True) (Just TdSyncIncremental) (Just False) (Just False) (Just (SaveOptions (Just False)))

completionProvider               :: CompletionOptions
completionProvider               = CompletionOptions (Just False) (Just [])

signatureHelpProvider            :: Maybe SignatureHelpOptions
signatureHelpProvider            = Nothing

typeDefinitionProvider           :: Maybe GotoOptions
typeDefinitionProvider           = Nothing

implementationProvider           :: Maybe GotoOptions
implementationProvider           = Nothing

codeActionProvider               :: Maybe CodeActionOptions
codeActionProvider               = Nothing

codeLensProvider                 :: Maybe CodeLensOptions
codeLensProvider                 = Nothing

documentOnTypeFormattingProvider :: Maybe DocumentOnTypeFormattingOptions
documentOnTypeFormattingProvider = Nothing

renameProvider                   :: RenameOptions
renameProvider                   = RenameOptions (Just False)

documentLinkProvider             :: Maybe DocumentLinkOptions
documentLinkProvider             = Nothing

colorProvider                    :: Maybe ColorOptions
colorProvider                    = Nothing

foldingRangeProvider             :: Maybe FoldingRangeOptions
foldingRangeProvider             = Nothing

executeCommandProvider           :: Maybe ExecuteCommandOptions
executeCommandProvider           = Nothing

-- Main
main :: IO ()
main = do
  ch <- atomically newTMChan
  _ <- runWithHandles
    stdin
    stdout
    InitializeCallbacks
      { onInitialConfiguration = Main.onInitialConfiguration
      , onConfigurationChange  = Main.onConfigurationChange
      , onStartup              = Main.onStartup ch
      }
    Handlers
      { hoverHandler                                 = Just . Main.hoverHandler $ ch
      , completionHandler                            = Just Main.completionHandler
      , completionResolveHandler                     = Just Main.completionResolveHandler
      , signatureHelpHandler                         = Main.signatureHelpHandler
      , definitionHandler                            = Main.definitionHandler
      , typeDefinitionHandler                        = Main.typeDefinitionHandler
      , implementationHandler                        = Main.implementationHandler
      , referencesHandler                            = Main.referencesHandler
      , documentHighlightHandler                     = Main.documentHighlightHandler
      , documentSymbolHandler                        = Main.documentSymbolHandler
      , workspaceSymbolHandler                       = Main.workspaceSymbolHandler
      , codeActionHandler                            = Main.codeActionHandler
      , codeLensHandler                              = Main.codeLensHandler
      , codeLensResolveHandler                       = Main.codeLensResolveHandler
      , documentColorHandler                         = Main.documentColorHandler
      , colorPresentationHandler                     = Main.colorPresentationHandler
      , documentFormattingHandler                    = Main.documentFormattingHandler
      , documentRangeFormattingHandler               = Main.documentRangeFormattingHandler
      , documentTypeFormattingHandler                = Main.documentTypeFormattingHandler
      , renameHandler                                = Just Main.renameHandler
      , prepareRenameHandler                         = Main.prepareRenameHandler
      , foldingRangeHandler                          = Main.foldingRangeHandler
      , documentLinkHandler                          = Main.documentLinkHandler
      , documentLinkResolveHandler                   = Main.documentLinkResolveHandler
      , executeCommandHandler                        = Main.executeCommandHandler
      , willSaveWaitUntilTextDocHandler              = Main.willSaveWaitUntilTextDocHandler
      , didChangeConfigurationParamsHandler          = Main.didChangeConfigurationParamsHandler
      , didOpenTextDocumentNotificationHandler       = Just . Main.didOpenTextDocumentNotificationHandler $ ch
      , didChangeTextDocumentNotificationHandler     = Just . Main.didChangeTextDocumentNotificationHandler $ ch
      , didCloseTextDocumentNotificationHandler      = Just . Main.didCloseTextDocumentNotificationHandler $ ch
      , didSaveTextDocumentNotificationHandler       = Just . Main.didSaveTextDocumentNotificationHandler $ ch
      , didChangeWatchedFilesNotificationHandler     = Main.didChangeWatchedFilesNotificationHandler
      , didChangeWorkspaceFoldersNotificationHandler = Main.didChangeWorkspaceFoldersNotificationHandler
      , initializedHandler                           = Just Main.initializedHandler
      , willSaveTextDocumentNotificationHandler      = Main.willSaveTextDocumentNotificationHandler
      , cancelNotificationHandler                    = Just Main.cancelNotificationHandler
      , responseHandler                              = Just Main.responseHandler
      , initializeRequestHandler                     = Just Main.initializeRequestHandler
      , exitNotificationHandler                      = Main.exitNotificationHandler
      , customRequestHandler                         = Main.customRequestHandler
      , customNotificationHandler                    = Main.customNotificationHandler
      }
    Options
      { textDocumentSync                 = Just Main.textDocumentSync
      , completionProvider               = Just Main.completionProvider
      , signatureHelpProvider            = Main.signatureHelpProvider
      , typeDefinitionProvider           = Main.typeDefinitionProvider
      , implementationProvider           = Main.implementationProvider
      , codeActionProvider               = Main.codeActionProvider
      , codeLensProvider                 = Main.codeLensProvider
      , documentOnTypeFormattingProvider = Main.documentOnTypeFormattingProvider
      , renameProvider                   = Just Main.renameProvider
      , documentLinkProvider             = Main.documentLinkProvider
      , colorProvider                    = Main.colorProvider
      , foldingRangeProvider             = Main.foldingRangeProvider
      , executeCommandProvider           = Main.executeCommandProvider
      }
    (Just "lang_server.log")
  return ()

