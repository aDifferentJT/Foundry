{-# LANGUAGE FlexibleInstances, FlexibleContexts, LambdaCase, MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings, TupleSections, RecordWildCards #-}
{-|
Module      : Parser.Monad
Description : The monad threaded through the parser
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental

This is the monad that is threaded through the lexer and the parser
-}
module Parser.Monad
  ( alexMove
  , AlexInput(AlexInput, charPos, charPrev, pending, str)
  , ignorePendingBytes
  , alexGetByte
  , alexInputPrevChar
  , Defn(RegDefn, InstDefn, ButtonDefn, MemoryDefn)
  , ParserState(..)
  , initialParserState
  , ParserMonad
  , prettyPosn
  , Locatable(Locatable)
  , throwLocalError'
  , throwLocalError
  , throwLocalErrorAt'
  , throwLocalErrorAt
  , throwGlobalError
  , recover
  , unrecover
  , defineLocalVar
  , defineReg
  , defineInst
  , defineButton
  , defineMemory
  , addImplemented
  , addEncoded
  , throwUnimpl
  , checkLocalVar
  , checkRegDefined
  , checkInstDefined
  , checkButtonDefined
  , getIdentifierDefn
  , getLocalVarEncWidth
  , getLocalVarValWidth
  , getMemoryWidth
  , clearLocalVars
  , defineEncType
  , getEncWidth
  , unmaybeBitsExpr
  , encPrefix
  , makeOpExpr
  , makeTernaryExpr
  , runParser'
  , runParser
  ) where

import ClassyPrelude

import Bits (Bit)
import Parser.AlexPosn
  ( AlexPosn(AlexPosn)
  , Locatable(Locatable, locatableValue, locatablePosns)
  )

import Parser.AST
  ( Type(..)
  , EncType(EncType)
  , MaybeBitsExpr(..)
  , BitsExpr(..)
  , sizeOfEnc
  , Op(..)
  , BoolExpr
  , Expr(..)
  , widthOfExpr
  , Memory(Memory)
  , RegType
  , InstType
  , ButtonType
  )
import Parser.Errors hiding (recover, unrecover)
import qualified Parser.Errors as Errors
import Utils (mapLeft)

import Control.Arrow ((***))
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.State (StateT, runStateT, mapStateT, get)
import qualified Control.Monad.Trans.State as State

import Data.Char (ord)
import qualified Data.Bits
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Data.Word (Word8)

-- | Encode a Haskell Text to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> (Word8, [Word8])
utf8Encode = (fromIntegral *** map fromIntegral) . go . ord
 where go :: Int -> (Int, [Int])
       go oc
         | oc <= 0x7f       = (oc, [])
         | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                              , [0x80 + oc Data.Bits..&. 0x3f]
                              )
         | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                              , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]
                              )
         | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                              , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                                , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]
                              )

-- | The starting position
alexStartPos :: AlexPosn
alexStartPos = AlexPosn 0 1 1

-- | The width of a tab - used for column numbers
alexTabSize :: Int
alexTabSize = 8

-- | Advance the position by the given character
alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPosn a l c) '\t' = AlexPosn (a + 1)  l       (c + alexTabSize - ((c - 1) `mod` alexTabSize))
alexMove (AlexPosn a l _) '\n' = AlexPosn (a + 1) (l + 1)   1
alexMove (AlexPosn a l c) _    = AlexPosn (a + 1)  l       (c + 1)

-- | The current input to the lexer
data AlexInput = AlexInput
  { charPos  :: AlexPosn -- ^ The position of the character
  , charPrev :: Char     -- ^ The previous character
  , pending  :: [Word8]  -- ^ The pending bytes on the current character
  , str      :: Text     -- ^ The current input string
  }

-- | Ignore any remaining bytes in the current character
ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes input = input { pending = [] }

-- | Return the previously read character
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = charPrev

-- | Get another byte from the input
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput{..} = case (pending, uncons str) of
  (b:bs, _         ) -> Just (b,AlexInput{ pending = bs, .. })
  ([]  , Nothing   ) -> Nothing
  ([]  , Just (c,s)) ->
    let p = alexMove charPos c in
    let (b, bs) = utf8Encode c in
    p `seq`  Just (b, AlexInput p c bs s)

-- | A definition
data Defn
  = RegDefn Int
  | InstDefn [Type]
  | ButtonDefn
  | MemoryDefn Int Int

-- | The current state to be threaded through the parser in the monad
data ParserState = ParserState
  { stateInput        :: AlexInput                                   -- ^ The input to be read
  , stateLocalVars    :: Map Text Type                               -- ^ The currently in scope local variables
  , stateGlobalIdents :: Map Text (Locatable Defn)                   -- ^ The global variables that have been defined so far
  , stateToBeImpl     :: Map Text (Text, Maybe (AlexPosn, AlexPosn)) -- ^ The implementations that are still needed and the errors if they are not
  , stateToBeEnc      :: Map Text (Text, Maybe (AlexPosn, AlexPosn)) -- ^ The encodings that are still needed and the errors if they are not
  , stateEncWidths    :: Map Type Int                                -- ^ The encoding types that have so far been defined
  }

-- | Identifiers that should be predefined
initialIdents :: Map Text (Locatable Defn)
initialIdents = Map.fromList
  [ ("always", pure $ InstDefn [])
  ]

-- | The state to be threaded into the beginning of the parser
initialParserState :: Text -> ParserState
initialParserState s = ParserState
    (AlexInput alexStartPos '\n' [] s)
    Map.empty
    initialIdents
    (Map.mapWithKey getInitialToBeImpl initialIdents)
    Map.empty
    Map.empty
  where getInitialToBeImpl :: Text -> Locatable Defn -> (Text, Maybe (AlexPosn, AlexPosn))
        getInitialToBeImpl v Locatable{..} =
          ( ( case locatableValue of
              RegDefn _      -> "Register "
              InstDefn _     -> "Instruction "
              ButtonDefn     -> "Button "
              MemoryDefn _ _ -> "Memory "
              ) ++ v ++ " has no implementation"
            , locatablePosns
            )

-- | The monad to be threaded through the lexer and the parser
type ParserMonad a = StateT ParserState (Errors (Text, Maybe (AlexPosn, AlexPosn))) a

-- | Print a position in a nice way
prettyPosn :: Maybe (AlexPosn, AlexPosn) -> Text
prettyPosn (Just (AlexPosn _ l1 c1, AlexPosn _ l2 c2))
  | l1 == l2  = "Line " ++ tshow l1 ++ " Column " ++ tshow c1 ++ "-" ++ tshow c2
  | otherwise = "Line " ++ tshow l1 ++ " Column " ++ tshow c1 ++ " to Line " ++ tshow l2 ++ " Column " ++ tshow c2
prettyPosn Nothing = "Unknown Position"

-- | Throw an error in the `ParserMonad' in the location of the given `Locatable' without recovering
throwLocalError' :: Locatable a -> Text -> ParserMonad b
throwLocalError' (Locatable _ ps) s = throwError (s, ps)

-- | Throw an error in the `ParserMonad' in the location of the given `Locatable'
throwLocalError :: b -> Locatable a -> Text -> ParserMonad b
throwLocalError x l = recover x . throwLocalError' l

-- | Throw an error in the `ParserMonad' in the given location without recovering
throwLocalErrorAt' :: Maybe (AlexPosn, AlexPosn) -> Text -> ParserMonad b
throwLocalErrorAt' ps s = throwError (s, ps)

-- | Throw an error in the `ParserMonad' in the given location
throwLocalErrorAt :: b -> Maybe (AlexPosn, AlexPosn) -> Text -> ParserMonad b
throwLocalErrorAt x ps = recover x . throwLocalErrorAt' ps

-- | Throw an error in the `ParserMonad' without a location
throwGlobalError :: MonadError (Text, Maybe (AlexPosn, AlexPosn)) m => Text -> m a
throwGlobalError = throwError . (, Nothing)

-- | Recover from an error in the `ParserMonad`
recover :: a -> ParserMonad a -> ParserMonad a
recover x m = do
  s <- get
  mapStateT (Errors.recover (x, s)) m

-- | Bring errors to the surface in the `ParserMonad`
unrecover :: ParserMonad a -> ParserMonad a
unrecover = mapStateT Errors.unrecover

-- | Define a local variable with the given identifier and type in the `ParserMonad'
defineLocalVar :: Locatable Text -> Type -> ParserMonad (Locatable Text)
defineLocalVar Locatable{..} t = do
  ParserState{..} <- State.get
  when (Map.member locatableValue stateLocalVars)    . throwLocalErrorAt () locatablePosns $ "Variable " ++ locatableValue ++ " redefined"
  when (Map.member locatableValue stateGlobalIdents) . throwLocalErrorAt () locatablePosns $ "Variable " ++ locatableValue ++ " has the same name as an identifier previously defined at " ++ prettyPosn locatablePosns
  State.put $ ParserState{ stateLocalVars = Map.insert locatableValue t stateLocalVars, .. }
  return Locatable{..}

-- | Define a register with the given identifier and width in the `ParserMonad'
defineReg :: Locatable Text -> Locatable Int -> ParserMonad (Locatable RegType)
defineReg var n = do
  ParserState{..} <- State.get
  case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) -> throwLocalError () var $
      "Register " ++ locatableValue var ++ " has the same name as a previously defined register, defined at " ++ prettyPosn ps
    Just (Locatable (InstDefn _)     ps) -> throwLocalError () var $
      "Register " ++ locatableValue var ++ " has the same name as a previously defined instruction, defined at " ++ prettyPosn ps
    Just (Locatable  ButtonDefn      ps) -> throwLocalError () var $
      "Register " ++ locatableValue var ++ " has the same name as a previously defined button, defined at " ++ prettyPosn ps
    Just (Locatable (MemoryDefn _ _) ps) -> throwLocalError () var $
      "Register " ++ locatableValue var ++ " has the same name as a previously defined memory, defined at " ++ prettyPosn ps
    Nothing               -> State.put $
      ParserState{ stateGlobalIdents = Map.insert (locatableValue var) (RegDefn <$ var <*> n) stateGlobalIdents, .. }
  return $ (,) <$> var <*> n

-- | Define an instruction with the given identifier and arguments in the `ParserMonad'
defineInst :: Locatable Text -> Locatable [Type] -> ParserMonad (Locatable InstType)
defineInst var ts = do
  ParserState{..} <- State.get
  case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) -> throwLocalError () var
      $ "Instruction " ++ locatableValue var ++ " has the same name as a previously defined register, defined at " ++ prettyPosn ps
    Just (Locatable (InstDefn _)     ps) -> throwLocalError () var
      $ "Instruction " ++ locatableValue var ++ " has the same name as a previously defined instruction, defined at " ++ prettyPosn ps
    Just (Locatable  ButtonDefn      ps) -> throwLocalError () var
      $ "Instruction " ++ locatableValue var ++ " has the same name as a previously defined button, defined at " ++ prettyPosn ps
    Just (Locatable (MemoryDefn _ _) ps) -> throwLocalError () var
      $ "Instruction " ++ locatableValue var ++ " has the same name as a previously defined memory, defined at " ++ prettyPosn ps
    Nothing               -> State.put
      $ ParserState
      { stateGlobalIdents = Map.insert (locatableValue var) (InstDefn <$ var <*> ts) stateGlobalIdents
      , stateToBeImpl     = Map.insert (locatableValue var) ("Instruction " ++ locatableValue var ++ " has no implementation", locatablePosns var) stateToBeImpl
      , stateToBeEnc      = Map.insert (locatableValue var) ("Instruction " ++ locatableValue var ++ " has no encoding",       locatablePosns var) stateToBeEnc
      , ..
      }
  return $ (,) <$> var <*> ts

-- | Define a button with the given identifier and physical id in the `ParserMonad'
defineButton :: Locatable Text -> Locatable Int -> ParserMonad (Locatable ButtonType)
defineButton var n = do
  ParserState{..} <- State.get
  case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) -> throwLocalError () var
      $ "Button " ++ locatableValue var ++ " has the same name as a previously defined register, defined at " ++ prettyPosn ps
    Just (Locatable (InstDefn _)     ps) -> throwLocalError () var
      $ "Button " ++ locatableValue var ++ " has the same name as a previously defined instruction, defined at " ++ prettyPosn ps
    Just (Locatable  ButtonDefn      ps) -> throwLocalError () var
      $ "Button " ++ locatableValue var ++ " has the same name as a previously defined button, defined at " ++ prettyPosn ps
    Just (Locatable (MemoryDefn _ _) ps) -> throwLocalError () var
      $ "Button " ++ locatableValue var ++ " has the same name as a previously defined memory, defined at " ++ prettyPosn ps
    Nothing               -> State.put
      $ ParserState
      { stateGlobalIdents = Map.insert (locatableValue var) (ButtonDefn <$ var) stateGlobalIdents
      , stateToBeImpl = Map.insert (locatableValue var) ("Button " ++ locatableValue var ++ " has no implementation", locatablePosns var) stateToBeImpl
      , ..
      }
  return $ (,) <$> var <*> n

-- | Define a memory with the given identifier, data width and address width in the `ParserMonad'
defineMemory :: Locatable Text -> Locatable Int -> Locatable Int -> ParserMonad (Locatable Memory)
defineMemory var dataWidth addressWidth = do
  ParserState{..} <- State.get
  case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) -> throwLocalError () var
      $ "Memory " ++ locatableValue var ++ " has the same name as a previously defined register, defined at " ++ prettyPosn ps
    Just (Locatable (InstDefn _)     ps) -> throwLocalError () var
      $ "Memory " ++ locatableValue var ++ " has the same name as a previously defined instruction, defined at " ++ prettyPosn ps
    Just (Locatable  ButtonDefn      ps) -> throwLocalError () var
      $ "Memory " ++ locatableValue var ++ " has the same name as a previously defined button, defined at " ++ prettyPosn ps
    Just (Locatable (MemoryDefn _ _) ps) -> throwLocalError () var
      $ "Memory " ++ locatableValue var ++ " has the same name as a previously defined memory, defined at " ++ prettyPosn ps
    Nothing               -> State.put
      $ ParserState{ stateGlobalIdents = Map.insert (locatableValue var) (MemoryDefn <$ var <*> dataWidth <*> addressWidth) stateGlobalIdents, .. }
  return $ Memory <$> var <*> dataWidth <*> addressWidth

addImplemented :: Locatable Text -> ParserMonad ()
addImplemented Locatable{..} = do
  ParserState{..} <- State.get
  unless (Map.member locatableValue stateToBeImpl)
    . throwLocalErrorAt () locatablePosns
    $ locatableValue ++ " already implemented"
  State.put $ ParserState{ stateToBeImpl = Map.delete locatableValue stateToBeImpl, .. }

addEncoded :: Locatable Text -> ParserMonad ()
addEncoded Locatable{..} = do
  ParserState{..} <- State.get
  unless (Map.member locatableValue stateToBeEnc)
    . throwLocalErrorAt () locatablePosns
    $ locatableValue ++ " already encoded"
  State.put $ ParserState{ stateToBeEnc = Map.delete locatableValue stateToBeEnc, .. }

throwUnimpl :: ParserMonad ()
throwUnimpl = do
  ParserState{..} <- State.get
  lift . throwErrors () $ Map.elems stateToBeImpl ++ Map.elems stateToBeEnc

-- | Check if the given identifier represents a local variable in the current scope
checkLocalVar :: Locatable Text -> ParserMonad ()
checkLocalVar var = do
  ParserState{..} <- State.get
  if Map.member (locatableValue var) stateLocalVars
  then return ()
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) ->
      throwLocalError () var $ locatableValue var ++ " is a register as defined at " ++ prettyPosn ps ++ ", expected a local variable"
    Just (Locatable (InstDefn _)     ps) ->
      throwLocalError () var $ locatableValue var ++ " is an instruction as defined at " ++ prettyPosn ps ++ ", expected a local variable"
    Just (Locatable  ButtonDefn      ps) ->
      throwLocalError () var $ locatableValue var ++ " is a button as defined at " ++ prettyPosn ps ++ ", expected a local variable"
    Just (Locatable (MemoryDefn _ _) ps) ->
      throwLocalError () var $ locatableValue var ++ " is a memory as defined at " ++ prettyPosn ps ++ ", expected a local variable"
    Nothing               ->
      throwLocalError () var $ "Variable " ++ locatableValue var ++ " not defined"

-- | Check if the given identifier represents a register
checkRegDefined :: Locatable Text -> ParserMonad ()
checkRegDefined var = do
  ParserState{..} <- State.get
  if Map.member (locatableValue var) stateLocalVars
  then throwLocalError () var $ locatableValue var ++ " is a local variable, expected a register"
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      _)  -> return ()
    Just (Locatable (InstDefn _)     ps) ->
      throwLocalError () var $ locatableValue var ++ " is an instruction as defined at " ++ prettyPosn ps ++ ", expected a register"
    Just (Locatable  ButtonDefn      ps) ->
      throwLocalError () var $ locatableValue var ++ " is a button as defined at " ++ prettyPosn ps ++ ", expected a register"
    Just (Locatable (MemoryDefn _ _) ps) ->
      throwLocalError () var $ locatableValue var ++ " is a memory as defined at " ++ prettyPosn ps ++ ", expected a register"
    Nothing               ->
      throwLocalError () var $ "Register " ++ locatableValue var ++ " not defined"

-- | Check if the given identifier represents an instruction
checkInstDefined :: Locatable Text -> ParserMonad ()
checkInstDefined var = do
  ParserState{..} <- State.get
  if Map.member (locatableValue var) stateLocalVars
  then throwLocalError () var $ locatableValue var ++ " is a local variable, expected an instruction"
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) ->
      throwLocalError () var $ locatableValue var ++ " is a register as defined at " ++ prettyPosn ps ++ ", expected an instruction"
    Just (Locatable (InstDefn _)     _)  -> return ()
    Just (Locatable  ButtonDefn      ps) ->
      throwLocalError () var $ locatableValue var ++ " is a button as defined at " ++ prettyPosn ps ++ ", expected an instruction"
    Just (Locatable (MemoryDefn _ _) ps) ->
      throwLocalError () var $ locatableValue var ++ " is a memory as defined at " ++ prettyPosn ps ++ ", expected an instruction"
    Nothing               ->
      throwLocalError () var $ "Instruction " ++ locatableValue var ++ " not defined"

-- | Check if the given identifier represents a button
checkButtonDefined :: Locatable Text -> ParserMonad ()
checkButtonDefined var = do
  ParserState{..} <- State.get
  if Map.member (locatableValue var) stateLocalVars
  then throwLocalError () var $ locatableValue var ++ " is a local variable, expected a button"
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) ->
      throwLocalError () var $ locatableValue var ++ " is a register as defined at " ++ prettyPosn ps ++ ", expected a button"
    Just (Locatable (InstDefn _)     ps) ->
      throwLocalError () var $ locatableValue var ++ " is an instruction as defined at " ++ prettyPosn ps ++ ", expected a button"
    Just (Locatable  ButtonDefn      _)  -> return ()
    Just (Locatable (MemoryDefn _ _) ps) ->
      throwLocalError () var $ locatableValue var ++ " is a memory as defined at " ++ prettyPosn ps ++ ", expected a button"
    Nothing               ->
      throwLocalError () var $ "Button " ++ locatableValue var ++ " not defined"

-- | Get the definition of the given identifier
getIdentifierDefn :: Locatable Text -> ParserMonad (Locatable (Maybe Defn))
getIdentifierDefn Locatable{..} = do
  ParserState{..} <- State.get
  case Map.lookup locatableValue stateGlobalIdents of
    Nothing -> throwLocalError (Nothing <$ Locatable{..}) Locatable{..} $ "Identifier " ++ locatableValue ++ " not defined"
    Just d  -> return $ Just <$> d

-- | Get the width of the encoding of the given local variable
getLocalVarEncWidth :: Locatable Text -> ParserMonad (Maybe Int)
getLocalVarEncWidth var =
  runMaybeT $ MaybeT (Map.lookup (locatableValue var) . stateLocalVars <$> State.get) >>= MaybeT . getEncWidth . pure

-- | Get the width of the encoding of the value contained in the given local variable
getLocalVarValWidth :: Locatable Text -> ParserMonad (Maybe Int)
getLocalVarValWidth var =
  runMaybeT $ MaybeT (Map.lookup (locatableValue var) . stateLocalVars <$> State.get) >>= MaybeT . getValWidth . pure

-- | Check if the given identifier represents a memory
getMemoryWidth :: Locatable Text -> ParserMonad (Maybe Int)
getMemoryWidth var = do
  ParserState{..} <- State.get
  if Map.member (locatableValue var) stateLocalVars
  then throwLocalError Nothing var $ locatableValue var ++ " is a local variable, expected a memory"
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) ->
      throwLocalError Nothing var $ locatableValue var ++ " is a register as defined at " ++ prettyPosn ps ++ ", expected a memory"
    Just (Locatable (InstDefn _)     ps) ->
      throwLocalError Nothing var $ locatableValue var ++ " is an instruction as defined at " ++ prettyPosn ps ++ ", expected a memory"
    Just (Locatable  ButtonDefn      ps) ->
      throwLocalError Nothing var $ locatableValue var ++ " is a button as defined at " ++ prettyPosn ps ++ ", expected a memory"
    Just (Locatable (MemoryDefn n _) _)  -> return . Just $ n
    Nothing                              ->
      throwLocalError Nothing var $ "Memory " ++ locatableValue var ++ " not defined"

-- | Clear all the currently defined local variables
clearLocalVars :: ParserMonad ()
clearLocalVars = do
  ParserState{..} <- State.get
  State.put $ ParserState{ stateLocalVars = Map.empty, .. }

-- | Define the type of an encoding in the `ParserMonad'
defineEncType :: Locatable Type -> Locatable Int -> ParserMonad (Locatable EncType)
defineEncType t n = do
  ParserState{..} <- State.get
  when (Map.member (locatableValue t) stateEncWidths) . throwLocalError () t $ "Encoding of type " ++ tshow t ++ " redefined"
  State.put $ ParserState{ stateEncWidths = Map.insert (locatableValue t) (locatableValue n) stateEncWidths, .. }
  return $ EncType <$> t <*> n

-- | Get the width of the given encoding
getEncWidth :: Locatable Type -> ParserMonad (Maybe Int)
getEncWidth Locatable{..} = case locatableValue of
  (BitsT n) -> return . Just $ n
  (IntT  n) -> return . Just $ n
  t         -> do
    ParserState{..} <- State.get
    case Map.lookup t stateEncWidths of
      Just n  -> return . Just $ n
      Nothing -> throwLocalError Nothing Locatable{..} $ "Encoding of type " ++ tshow t ++ " not defined"

-- | Get the width of the given encoding
getValWidth :: Locatable Type -> ParserMonad (Maybe Int)
getValWidth Locatable{..} = case locatableValue of
  (BitsT n) -> return . Just $ n
  (IntT  n) -> return . Just $ n
  (RegT  n) -> return . Just $ n
  InstT     -> throwLocalErrorAt Nothing locatablePosns "Instruction has no value"

unmaybeBitsExpr :: MaybeBitsExpr -> ParserMonad BitsExpr
unmaybeBitsExpr (MaybeConstBitsExpr bs)              = return $ ConstBitsExpr bs
unmaybeBitsExpr (MaybeEncBitsExpr    n v)     = return $ EncBitsExpr (fromMaybe 0 n) v
unmaybeBitsExpr (MaybeConcatBitsExpr n e1 e2) = ConcatBitsExpr (fromMaybe 0 n) <$> unmaybeBitsExpr e1 <*> unmaybeBitsExpr e2

-- | Get the constant prefix of the given expression
encPrefix :: BitsExpr -> ParserMonad ([Bit], BitsExpr)
encPrefix (ConstBitsExpr bs)       = return (bs, ConstBitsExpr [])
encPrefix (EncBitsExpr n v)        = return ([], EncBitsExpr n v)
encPrefix (ConcatBitsExpr _ e1 e2) = encPrefix e1 >>= \case
  (bs, ConstBitsExpr []) -> first (bs ++) <$> encPrefix e2
  (bs1, e1')             -> return (bs1, ConcatBitsExpr (sizeOfEnc e1' + sizeOfEnc e2) e1' e2)

makeOpExpr :: Op -> Locatable Expr -> Locatable Expr -> ParserMonad (Locatable Expr)
makeOpExpr ConcatBits e1 e2 =
  return $ OpExpr (liftM2 (+) (widthOfExpr . locatableValue $ e1) (widthOfExpr . locatableValue $ e2)) ConcatBits <$> e1 <*> e2
makeOpExpr op e1 e2         = do
  let e1' = locatableValue e1
  let e2' = locatableValue e2
  n <- case (widthOfExpr . locatableValue $ e1, widthOfExpr . locatableValue $ e2) of
    (Just n1, Just n2)
      | n1 == n2  -> return . Just $ n1
      | otherwise -> throwLocalError Nothing (e1 <* e2)
          $ "Cannot " ++ tshow op ++ " operands of widths " ++ tshow n1 ++ " and " ++ tshow n2
    (Just n,  Nothing) -> return . Just $ n
    (Nothing, Just n)  -> return . Just $ n
    (Nothing, Nothing) -> return Nothing
  return $ OpExpr n op <$> e1 <*> e2

makeTernaryExpr :: Locatable BoolExpr -> Locatable Expr -> Locatable Expr -> ParserMonad (Locatable Expr)
makeTernaryExpr c e1 e2 = do
  n <- case (widthOfExpr . locatableValue $ e1, widthOfExpr . locatableValue $ e2) of
    (Just n1, Just n2)
      | n1 == n2  -> return . Just $ n1
      | otherwise -> throwLocalError Nothing (e1 <* e2)
          $ "Cannot have ternary expression with operands of widths " ++ tshow n1 ++ " and " ++ tshow n2
    (Just n,  Nothing) -> return . Just $ n
    (Nothing, Just n)  -> return . Just $ n
    (Nothing, Nothing) -> return Nothing
  return $ TernaryExpr n (locatableValue c) <$> e1 <*> e2

runParser' :: ParserMonad a -> Text -> Either [(Text, Maybe (AlexPosn, AlexPosn))] a
runParser' m = runErrors . (fst <$>) . runStateT m . initialParserState

printErrors :: Text -> Either [(Text, Maybe (AlexPosn, AlexPosn))] a -> Either Text a
printErrors s = mapLeft (intercalate "\n" . map (printError s))
  where printError :: Text -> (Text, Maybe (AlexPosn, AlexPosn)) -> Text
        printError _ (e, Nothing) = e ++ "\n"
        printError s (e, Just (AlexPosn a1 l1 c1, AlexPosn a2 l2 c2))
          | l1 == l2 =
              let (line1, line2) =  (reverse . takeWhile (/= '\n') . reverse *** takeWhile (/= '\n')) . splitAt a1 $ s in
              prettyPosn (Just (AlexPosn a1 l1 c1, AlexPosn a2 l2 c2)) ++ ": " ++ e ++ "\n" ++ line1 ++ line2 ++ "\n" ++ replicate (c1 - 1) ' ' ++ replicate (c2 - c1) '^' ++ "\n"
          | otherwise =
              prettyPosn (Just (AlexPosn a1 l1 c1, AlexPosn a2 l2 c2)) ++ ": " ++ e ++ "\nError spanning mulitple lines, I don't yet know how to display that!!!\n"

-- | Run the given parser on the given input string and either return a nicely formatted error or the output of the parser
runParser :: ParserMonad a -> Text -> Either Text a
runParser m s = printErrors s $ runParser' m s
