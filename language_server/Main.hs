{-# LANGUAGE BlockArguments, LambdaCase, NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Main(main) where

import ClassyPrelude hiding (Handler)

import Bits (Endianness(Little), bitsToInt)
import Parser (parseM, parse')
import Parser.AlexPosn (AlexPosn(AlexPosn), Locatable(..))
import Parser.Errors (runErrors, forgive)
import Parser.Lexer (Token(..), readToken)
import Parser.Monad (Defn(..), ParserState(..), initialParserState, runParser')

import Data.Maybe (fromMaybe, listToMaybe)

import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad (when, unless, forever, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.State (runStateT)
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import qualified Data.Map as Map
import Data.Rope.UTF16 (toText, splitAtLine)
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

untilM :: Monad m => Int -> (a -> Bool) -> m a -> m (Maybe a)
untilM 0 _ _ = return Nothing
untilM n f m = m >>= \x -> if f x then return . Just $ x else untilM (n-1) f m

data Config = Config
  deriving Show

data ReactorInput
  = SendMessage FromServerMessage
  | ShowDiagnostics Uri
  | ShowHover HoverRequest
  | ShowCompletion CompletionRequest

toRange :: Maybe (AlexPosn, AlexPosn) -> Range
toRange (Just (AlexPosn _ l1 c1, AlexPosn _ l2 c2)) = Range (Position (l1 - 1) (c1 - 1)) (Position (l2 - 1) (c2 - 1))
toRange Nothing                                     = Range (Position 0 0) (Position 0 0)

getTokenInformation :: Token -> Maybe Text
getTokenInformation (Bits (Locatable bs _)) = Just $
  "0b" ++ (mconcat . map tshow $ bs) ++ " = " ++ (tshow . bitsToInt Little $ bs)
getTokenInformation _ = Nothing

reactor :: LspFuncs Config -> TMChan ReactorInput -> IO ()
reactor LspFuncs{..} ch = forever . ((liftIO . atomically . readTMChan $ ch) >>=) $ \case
  Just (SendMessage msg) -> sendFunc msg
  Just (ShowDiagnostics uri) -> void . runMaybeT $ do
    VirtualFile version rope _ <- MaybeT . getVirtualFileFunc . toNormalizedUri $ uri
    case parse' . toText $ rope of
      Right _ -> lift . flushDiagnosticsBySourceFunc 0 . Just $ "Foundry"
      Left es -> do
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
              ]
        lift . publishDiagnosticsFunc 100 (toNormalizedUri uri) (Just version) $ ds
  Just (ShowHover req) -> void . runMaybeT $ do
    VirtualFile _ rope _ <- MaybeT . getVirtualFileFunc . toNormalizedUri $ req ^. params . textDocument . uri
    let Position lineNum col = req ^. params . position
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
  Just (ShowCompletion req) -> void . runMaybeT $ do
    VirtualFile _ rope _ <- MaybeT . getVirtualFileFunc . toNormalizedUri $ req ^. params . textDocument . uri
    ParserState{..} <- MaybeT . return . rightToMaybe . runErrors . forgive . (snd <$>) . runStateT parseM . initialParserState . toText . fst . splitAtLine (req ^. params . position . line + 1) $ rope
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
             ( Just $ case defn of
                 RegDefn  n     -> "A register of width " ++ tshow n
                 InstDefn _     -> "An instruction"
                 ButtonDefn     -> "A button" ++ tshow (req ^. params . position . line)
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
         | (var, Locatable defn defnPos) <- Map.toList stateGlobalIdents
         ]
    return ()
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
hoverHandler ch                              =
  atomically
    . writeTMChan ch
    . ShowHover

completionHandler                            :: TMChan ReactorInput -> Handler CompletionRequest
completionHandler ch                         =
  atomically
    . writeTMChan ch
    . ShowCompletion

completionResolveHandler                     :: Maybe (Handler CompletionItemResolveRequest)
completionResolveHandler                     = Nothing

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

renameHandler                                :: Maybe (Handler RenameRequest)
renameHandler                                = Nothing

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

cancelNotificationHandler                    :: Maybe (Handler CancelNotification)
cancelNotificationHandler                    = Nothing

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
      , completionHandler                            = Just . Main.completionHandler $ ch
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
      , cancelNotificationHandler                    = Main.cancelNotificationHandler
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

