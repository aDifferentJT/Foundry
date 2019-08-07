{-# LANGUAGE BlockArguments, RecordWildCards #-}

module Main(main) where

import Parser (parseFile)

import Data.Maybe (fromMaybe, listToMaybe)

import Control.Lens ((^.))
import Control.Monad (when, unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.LSP.Control
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Utility
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import System.IO (stdin, stdout)

data Config = Config
  deriving Show

-- InitializeCallbacks
onInitialConfiguration :: InitializeRequest -> Either Text Config
onInitialConfiguration m = Right Config

onConfigurationChange :: DidChangeConfigurationNotification -> Either Text Config
onConfigurationChange _ = Right Config

onStartup :: LspFuncs Config -> IO (Maybe ResponseError)
onStartup _ = return Nothing

-- Handlers
hoverHandler                                 :: Maybe (Handler HoverRequest)
hoverHandler                                 = Nothing

completionHandler                            :: Maybe (Handler CompletionRequest)
completionHandler                            = Nothing

completionResolveHandler                     :: Maybe (Handler CompletionItemResolveRequest)
completionResolveHandler                     = Nothing

signatureHelpHandler                         :: Maybe (Handler SignatureHelpRequest)
signatureHelpHandler                         = Nothing

definitionHandler                            :: Maybe (Handler DefinitionRequest)
definitionHandler                            = Just \req -> do
  let _ = req ^. method
  return ()

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

renameHandler                                :: Maybe (Handler RenameRequest)
renameHandler                                = Nothing

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

didOpenTextDocumentNotificationHandler       :: Maybe (Handler DidOpenTextDocumentNotification)
didOpenTextDocumentNotificationHandler       = Nothing

didChangeTextDocumentNotificationHandler     :: Maybe (Handler DidChangeTextDocumentNotification)
didChangeTextDocumentNotificationHandler     = Nothing
-- ^ Note: If you need to keep track of document changes,
-- "Language.Haskell.LSP.VFS" will take care of these messages for you!

didCloseTextDocumentNotificationHandler      :: Maybe (Handler DidCloseTextDocumentNotification)
didCloseTextDocumentNotificationHandler      = Nothing

didSaveTextDocumentNotificationHandler       :: Maybe (Handler DidSaveTextDocumentNotification)
didSaveTextDocumentNotificationHandler       = Nothing

didChangeWatchedFilesNotificationHandler     :: Maybe (Handler DidChangeWatchedFilesNotification)
didChangeWatchedFilesNotificationHandler     = Nothing

didChangeWorkspaceFoldersNotificationHandler :: Maybe (Handler DidChangeWorkspaceFoldersNotification)
didChangeWorkspaceFoldersNotificationHandler = Nothing

initializedHandler                           :: Maybe (Handler InitializedNotification)
initializedHandler                           = Nothing

willSaveTextDocumentNotificationHandler      :: Maybe (Handler WillSaveTextDocumentNotification)
willSaveTextDocumentNotificationHandler      = Nothing

cancelNotificationHandler                    :: Maybe (Handler CancelNotification)
cancelNotificationHandler                    = Nothing

responseHandler                              :: Maybe (Handler BareResponseMessage)
responseHandler                              = Just \resp -> do
  logs $ "Received response: " ++ show resp

initializeRequestHandler                     :: Maybe (Handler InitializeRequest)
initializeRequestHandler                     = Nothing

exitNotificationHandler                      :: Maybe (Handler ExitNotification)
exitNotificationHandler                      = Nothing

customRequestHandler                         :: Maybe (Handler CustomClientRequest)
customRequestHandler                         = Nothing

customNotificationHandler                    :: Maybe (Handler CustomClientNotification)
customNotificationHandler                    = Nothing

-- Options
textDocumentSync                 :: Maybe TextDocumentSyncOptions
textDocumentSync                 = Nothing

completionProvider               :: Maybe CompletionOptions
completionProvider               = Nothing

signatureHelpProvider            :: Maybe SignatureHelpOptions
signatureHelpProvider            = Nothing

typeDefinitionProvider           :: Maybe GotoOptions
typeDefinitionProvider           = Nothing

implementationProvider           :: Maybe GotoOptions
implementationProvider           = Nothing

codeLensProvider                 :: Maybe CodeLensOptions
codeLensProvider                 = Nothing

documentOnTypeFormattingProvider :: Maybe DocumentOnTypeFormattingOptions
documentOnTypeFormattingProvider = Nothing

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
  _ <- runWithHandles
    stdin
    stdout
    InitializeCallbacks
      { onInitialConfiguration = Main.onInitialConfiguration
      , onConfigurationChange  = Main.onConfigurationChange
      , onStartup              = Main.onStartup
      }
    Handlers
      { hoverHandler                                 = Main.hoverHandler
      , completionHandler                            = Main.completionHandler
      , completionResolveHandler                     = Main.completionResolveHandler
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
      , renameHandler                                = Main.renameHandler
      , foldingRangeHandler                          = Main.foldingRangeHandler
      , documentLinkHandler                          = Main.documentLinkHandler
      , documentLinkResolveHandler                   = Main.documentLinkResolveHandler
      , executeCommandHandler                        = Main.executeCommandHandler
      , willSaveWaitUntilTextDocHandler              = Main.willSaveWaitUntilTextDocHandler
      , didChangeConfigurationParamsHandler          = Main.didChangeConfigurationParamsHandler
      , didOpenTextDocumentNotificationHandler       = Main.didOpenTextDocumentNotificationHandler
      , didChangeTextDocumentNotificationHandler     = Main.didChangeTextDocumentNotificationHandler
      , didCloseTextDocumentNotificationHandler      = Main.didCloseTextDocumentNotificationHandler
      , didSaveTextDocumentNotificationHandler       = Main.didSaveTextDocumentNotificationHandler
      , didChangeWatchedFilesNotificationHandler     = Main.didChangeWatchedFilesNotificationHandler
      , didChangeWorkspaceFoldersNotificationHandler = Main.didChangeWorkspaceFoldersNotificationHandler
      , initializedHandler                           = Main.initializedHandler
      , willSaveTextDocumentNotificationHandler      = Main.willSaveTextDocumentNotificationHandler
      , cancelNotificationHandler                    = Main.cancelNotificationHandler
      , responseHandler                              = Main.responseHandler
      , initializeRequestHandler                     = Main.initializeRequestHandler
      , exitNotificationHandler                      = Main.exitNotificationHandler
      , customRequestHandler                         = Main.customRequestHandler
      , customNotificationHandler                    = Main.customNotificationHandler
      }
    Options
      { textDocumentSync                 = Main.textDocumentSync
      , completionProvider               = Main.completionProvider
      , signatureHelpProvider            = Main.signatureHelpProvider
      , typeDefinitionProvider           = Main.typeDefinitionProvider
      , implementationProvider           = Main.implementationProvider
      , codeLensProvider                 = Main.codeLensProvider
      , documentOnTypeFormattingProvider = Main.documentOnTypeFormattingProvider
      , documentLinkProvider             = Main.documentLinkProvider
      , colorProvider                    = Main.colorProvider
      , foldingRangeProvider             = Main.foldingRangeProvider
      , executeCommandProvider           = Main.executeCommandProvider
      }
    (Just "lang_server.log")
  return ()

