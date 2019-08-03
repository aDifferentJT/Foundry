{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TupleSections, RecordWildCards #-}

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
  , ParserState(ParserState, stateInput, stateLocalVars, stateGlobalIdents, stateEncTypes)
  , ParserMonad
  , Locatable(Locatable)
  , throwLocalError
  , throwLocalErrorAt
  , throwGlobalError
  , defineLocalVar
  , defineReg
  , defineInst
  , defineButton
  , defineMemory
  , isLocalVar
  , checkLocalVar
  , checkRegDefined
  , checkInstDefined
  , checkButtonDefined
  , checkMemoryDefined
  , getIdentifierDefn
  , clearLocalVars
  , defineEncType
  , getEncType
  , instEncDim
  , runParser
  ) where

import Parser.AlexPosn
  ( AlexPosn(AlexPosn)
  , Locatable(Locatable, locatableValue, locatablePosns)
  )

import Proc
  ( Type
    ( BitsT
    , IntT
    )
  , EncType(EncType)
  , BitsExpr
    ( ConstBitsExpr
    , EncBitsExpr
    , ConcatBitsExpr
    , AndBitsExpr
    , OrBitsExpr
    , XorBitsExpr
    )
  , Memory(Memory)
  )
import Parser.AST
  ( RegType(RegType)
  , InstType(InstType)
  , ButtonType(ButtonType)
  , UnsizedBitsExpr
    ( UnsizedConstBitsExpr
    , UnsizedEncBitsExpr
    , UnsizedConcatBitsExpr
    , UnsizedAndBitsExpr
    , UnsizedOrBitsExpr
    , UnsizedXorBitsExpr
    )
  )
import Utils (Bit)

import Control.Arrow ((***))
import Control.Monad (when, unless)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Control.Monad.Trans.State as State

import Data.Char (ord)
import qualified Data.Bits
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.Word (Word8)

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
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
  , str      :: String   -- ^ The current input string
  }

-- | Ignore any remaining bytes in the current character
ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes input = input { pending = [] }

-- | Return the previously read character
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = charPrev

-- | Get another byte from the input
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput{..} = case (pending, str) of
  (b:bs, _  ) -> Just (b,AlexInput{ pending = bs, .. })
  ([],   [] ) -> Nothing
  ([],   c:s) ->
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
  { stateInput        :: AlexInput                       -- ^ The input to be read
  , stateLocalVars    :: Set.Set String                  -- ^ The currently in scope local variables
  , stateGlobalIdents :: Map.Map String (Locatable Defn) -- ^ The global variables that have been defined so far
  , stateEncTypes     :: Map.Map Type Int                -- ^ The encoding types that have so far been defined
  }

-- | Identifiers that should be predefined
initialIdents :: Map.Map String (Locatable Defn)
initialIdents = Map.fromList
  [ ("always", pure $ InstDefn [])
  ]

-- | The state to be threaded into the beginning of the parser
initialParserState :: String -> ParserState
initialParserState s = ParserState (AlexInput alexStartPos '\n' [] s) Set.empty initialIdents Map.empty

-- | The monad to be threaded through the lexer and the parser
type ParserMonad a = StateT ParserState (Either (String, Maybe (AlexPosn, AlexPosn))) a

-- | Print a position in a nice way
prettyPosn :: Maybe (AlexPosn, AlexPosn) -> String
prettyPosn (Just (AlexPosn _ l1 c1, AlexPosn _ l2 c2))
  | l1 == l2  = "Line " ++ show l1 ++ " Column " ++ show c1 ++ "-" ++ show c2
  | otherwise = "Line " ++ show l1 ++ " Column " ++ show c1 ++ " to Line " ++ show l2 ++ " Column " ++ show c2
prettyPosn Nothing = "Unknown Position"

-- | Throw an error in the `ParserMonad' in the location of the given `Locatable'
throwLocalError :: Locatable a -> String -> ParserMonad b
throwLocalError (Locatable _ ps) s = throwError (s, ps)

-- | Throw an error in the `ParserMonad' in the given location
throwLocalErrorAt :: (AlexPosn, AlexPosn) -> String -> ParserMonad b
throwLocalErrorAt ps s = throwError (s, Just ps)

-- | Throw an error in the `ParserMonad' without a location
throwGlobalError :: MonadError (String, Maybe (AlexPosn, AlexPosn)) m => String -> m a
throwGlobalError = throwError . (, Nothing)

-- | Define a local variable with the given identifier in the `ParserMonad'
defineLocalVar :: Locatable String -> ParserMonad (Locatable String)
defineLocalVar Locatable{..} = do
  ParserState{..} <- State.get
  when (Set.member locatableValue stateLocalVars)    . throwLocalError Locatable{..} $ "Variable " ++ locatableValue ++ " redefined"
  when (Map.member locatableValue stateGlobalIdents) . throwLocalError Locatable{..} $ "Variable " ++ locatableValue ++ " has the same name as an identifier previously defined at " ++ prettyPosn locatablePosns
  State.put $ ParserState{ stateLocalVars = Set.insert locatableValue stateLocalVars, .. }
  return Locatable{..}

-- | Define a register with the given identifier and width in the `ParserMonad'
defineReg :: Locatable String -> Locatable Int -> ParserMonad (Locatable RegType)
defineReg var n = do
  ParserState{..} <- State.get
  case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) -> throwLocalError var $
      "Register " ++ locatableValue var ++ " has the same name as a previously defined register, defined at " ++ prettyPosn ps
    Just (Locatable (InstDefn _)     ps) -> throwLocalError var $
      "Register " ++ locatableValue var ++ " has the same name as a previously defined instruction, defined at " ++ prettyPosn ps
    Just (Locatable  ButtonDefn      ps) -> throwLocalError var $
      "Register " ++ locatableValue var ++ " has the same name as a previously defined button, defined at " ++ prettyPosn ps
    Just (Locatable (MemoryDefn _ _) ps) -> throwLocalError var $
      "Register " ++ locatableValue var ++ " has the same name as a previously defined memory, defined at " ++ prettyPosn ps
    Nothing               -> State.put $
      ParserState{ stateGlobalIdents = Map.insert (locatableValue var) (RegDefn <$ var <*> n) stateGlobalIdents, .. }
  return $ RegType <$> var <*> n

-- | Define an instruction with the given identifier and arguments in the `ParserMonad'
defineInst :: Locatable String -> Locatable [Type] -> ParserMonad (Locatable InstType)
defineInst var ts = do
  ParserState{..} <- State.get
  case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) -> throwLocalError var
      $ "Instruction " ++ locatableValue var ++ " has the same name as a previously defined register, defined at " ++ prettyPosn ps
    Just (Locatable (InstDefn _)     ps) -> throwLocalError var
      $ "Instruction " ++ locatableValue var ++ " has the same name as a previously defined instruction, defined at " ++ prettyPosn ps
    Just (Locatable  ButtonDefn      ps) -> throwLocalError var
      $ "Instruction " ++ locatableValue var ++ " has the same name as a previously defined button, defined at " ++ prettyPosn ps
    Just (Locatable (MemoryDefn _ _) ps) -> throwLocalError var
      $ "Instruction " ++ locatableValue var ++ " has the same name as a previously defined memory, defined at " ++ prettyPosn ps
    Nothing               -> State.put
      $ ParserState{ stateGlobalIdents = Map.insert (locatableValue var) (InstDefn <$ var <*> ts) stateGlobalIdents, .. }
  return $ InstType <$> var <*> ts

-- | Define a button with the given identifier and physical id in the `ParserMonad'
defineButton :: Locatable String -> Locatable Int -> ParserMonad (Locatable ButtonType)
defineButton var n = do
  ParserState{..} <- State.get
  case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) -> throwLocalError var
      $ "Button " ++ locatableValue var ++ " has the same name as a previously defined register, defined at " ++ prettyPosn ps
    Just (Locatable (InstDefn _)     ps) -> throwLocalError var
      $ "Button " ++ locatableValue var ++ " has the same name as a previously defined instruction, defined at " ++ prettyPosn ps
    Just (Locatable  ButtonDefn      ps) -> throwLocalError var
      $ "Button " ++ locatableValue var ++ " has the same name as a previously defined button, defined at " ++ prettyPosn ps
    Just (Locatable (MemoryDefn _ _) ps) -> throwLocalError var
      $ "Button " ++ locatableValue var ++ " has the same name as a previously defined memory, defined at " ++ prettyPosn ps
    Nothing               -> State.put
      $ ParserState{ stateGlobalIdents = Map.insert (locatableValue var) (ButtonDefn <$ var) stateGlobalIdents, .. }
  return $ ButtonType <$> var <*> n

-- | Define a memory with the given identifier, data width and address width in the `ParserMonad'
defineMemory :: Locatable String -> Locatable Int -> Locatable Int -> ParserMonad (Locatable Memory)
defineMemory var dataWidth addressWidth = do
  ParserState{..} <- State.get
  case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) -> throwLocalError var
      $ "Memory " ++ locatableValue var ++ " has the same name as a previously defined register, defined at " ++ prettyPosn ps
    Just (Locatable (InstDefn _)     ps) -> throwLocalError var
      $ "Memory " ++ locatableValue var ++ " has the same name as a previously defined instruction, defined at " ++ prettyPosn ps
    Just (Locatable  ButtonDefn      ps) -> throwLocalError var
      $ "Memory " ++ locatableValue var ++ " has the same name as a previously defined button, defined at " ++ prettyPosn ps
    Just (Locatable (MemoryDefn _ _) ps) -> throwLocalError var
      $ "Memory " ++ locatableValue var ++ " has the same name as a previously defined memory, defined at " ++ prettyPosn ps
    Nothing               -> State.put
      $ ParserState{ stateGlobalIdents = Map.insert (locatableValue var) (MemoryDefn <$ var <*> dataWidth <*> addressWidth) stateGlobalIdents, .. }
  return $ Memory <$> var <*> dataWidth <*> addressWidth

-- | Return if the given identifier represents a local variable in the current scope
isLocalVar :: String -> ParserMonad Bool
isLocalVar var = do
  ParserState{..} <- State.get
  return $ Set.member var stateLocalVars

-- | Check if the given identifier represents a local variable in the current scope
checkLocalVar :: Locatable String -> ParserMonad ()
checkLocalVar var = do
  ParserState{..} <- State.get
  if Set.member (locatableValue var) stateLocalVars
  then return ()
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) ->
      throwLocalError var $ locatableValue var ++ " is a register as defined at " ++ prettyPosn ps ++ ", expected a local variable"
    Just (Locatable (InstDefn _)     ps) ->
      throwLocalError var $ locatableValue var ++ " is an instruction as defined at " ++ prettyPosn ps ++ ", expected a local variable"
    Just (Locatable  ButtonDefn      ps) ->
      throwLocalError var $ locatableValue var ++ " is a button as defined at " ++ prettyPosn ps ++ ", expected a local variable"
    Just (Locatable (MemoryDefn _ _) ps) ->
      throwLocalError var $ locatableValue var ++ " is a memory as defined at " ++ prettyPosn ps ++ ", expected a local variable"
    Nothing               ->
      throwLocalError var $ "Variable " ++ locatableValue var ++ " not defined"

-- | Check if the given identifier represents a register
checkRegDefined :: Locatable String -> ParserMonad ()
checkRegDefined var = do
  ParserState{..} <- State.get
  if Set.member (locatableValue var) stateLocalVars
  then throwLocalError var $ locatableValue var ++ " is a local variable, expected a register"
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      _)  -> return ()
    Just (Locatable (InstDefn _)     ps) ->
      throwLocalError var $ locatableValue var ++ " is an instruction as defined at " ++ prettyPosn ps ++ ", expected a register"
    Just (Locatable  ButtonDefn      ps) ->
      throwLocalError var $ locatableValue var ++ " is a button as defined at " ++ prettyPosn ps ++ ", expected a register"
    Just (Locatable (MemoryDefn _ _) ps) ->
      throwLocalError var $ locatableValue var ++ " is a memory as defined at " ++ prettyPosn ps ++ ", expected a register"
    Nothing               ->
      throwLocalError var $ "Register " ++ locatableValue var ++ " not defined"

-- | Check if the given identifier represents an instruction
checkInstDefined :: Locatable String -> ParserMonad ()
checkInstDefined var = do
  ParserState{..} <- State.get
  if Set.member (locatableValue var) stateLocalVars
  then throwLocalError var $ locatableValue var ++ " is a local variable, expected an instruction"
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) ->
      throwLocalError var $ locatableValue var ++ " is a register as defined at " ++ prettyPosn ps ++ ", expected an instruction"
    Just (Locatable (InstDefn _)     _)  -> return ()
    Just (Locatable  ButtonDefn      ps) ->
      throwLocalError var $ locatableValue var ++ " is a button as defined at " ++ prettyPosn ps ++ ", expected an instruction"
    Just (Locatable (MemoryDefn _ _) ps) ->
      throwLocalError var $ locatableValue var ++ " is a memory as defined at " ++ prettyPosn ps ++ ", expected an instruction"
    Nothing               ->
      throwLocalError var $ "Instruction " ++ locatableValue var ++ " not defined"

-- | Check if the given identifier represents a button
checkButtonDefined :: Locatable String -> ParserMonad ()
checkButtonDefined var = do
  ParserState{..} <- State.get
  if Set.member (locatableValue var) stateLocalVars
  then throwLocalError var $ locatableValue var ++ " is a local variable, expected a button"
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) ->
      throwLocalError var $ locatableValue var ++ " is a register as defined at " ++ prettyPosn ps ++ ", expected a button"
    Just (Locatable (InstDefn _)     ps) ->
      throwLocalError var $ locatableValue var ++ " is an instruction as defined at " ++ prettyPosn ps ++ ", expected a button"
    Just (Locatable  ButtonDefn      _)  -> return ()
    Just (Locatable (MemoryDefn _ _) ps) ->
      throwLocalError var $ locatableValue var ++ " is a memory as defined at " ++ prettyPosn ps ++ ", expected a button"
    Nothing               ->
      throwLocalError var $ "Button " ++ locatableValue var ++ " not defined"

-- | Check if the given identifier represents a memory
checkMemoryDefined :: Locatable String -> ParserMonad ()
checkMemoryDefined var = do
  ParserState{..} <- State.get
  if Set.member (locatableValue var) stateLocalVars
  then throwLocalError var $ locatableValue var ++ " is a local variable, expected a memory"
  else case Map.lookup (locatableValue var) stateGlobalIdents of
    Just (Locatable (RegDefn _)      ps) ->
      throwLocalError var $ locatableValue var ++ " is a register as defined at " ++ prettyPosn ps ++ ", expected a memory"
    Just (Locatable (InstDefn _)     ps) ->
      throwLocalError var $ locatableValue var ++ " is an instruction as defined at " ++ prettyPosn ps ++ ", expected a memory"
    Just (Locatable  ButtonDefn      ps) ->
      throwLocalError var $ locatableValue var ++ " is a button as defined at " ++ prettyPosn ps ++ ", expected a memory"
    Just (Locatable (MemoryDefn _ _) _)  -> return ()
    Nothing                              ->
      throwLocalError var $ "Memory " ++ locatableValue var ++ " not defined"

-- | Get the definition of the given identifier
getIdentifierDefn :: Locatable String -> ParserMonad (Locatable Defn)
getIdentifierDefn Locatable{..} = do
  ParserState{..} <- State.get
  case Map.lookup locatableValue stateGlobalIdents of
    Nothing -> throwLocalError Locatable{..} $ "Identifier " ++ locatableValue ++ " not defined"
    Just d  -> return d

-- | Clear all the currently defined local variables
clearLocalVars :: ParserMonad ()
clearLocalVars = do
  ParserState{..} <- State.get
  State.put $ ParserState{ stateLocalVars = Set.empty, .. }

-- | Define the type of an encoding in the `ParserMonad'
defineEncType :: Locatable Type -> Locatable Int -> ParserMonad (Locatable EncType)
defineEncType t n = do
  ParserState{..} <- State.get
  when (Map.member (locatableValue t) stateEncTypes) . throwLocalError t $ "Encoding of type " ++ show t ++ " redefined"
  State.put $ ParserState{ stateEncTypes = Map.insert (locatableValue t) (locatableValue n) stateEncTypes, .. }
  return $ EncType <$> t <*> n

-- | Get the type of the given encoding
getEncType :: Locatable Type -> ParserMonad Int
getEncType Locatable{..} = case locatableValue of
  (BitsT n) -> return n
  (IntT n)  -> return n
  t         -> do
    ParserState{..} <- State.get
    case Map.lookup t stateEncTypes of
      Just n  -> return n
      Nothing -> throwLocalError Locatable{..} $ "Encoding of type " ++ show t ++ " not defined"

instEncDim' :: [Type] -> Locatable [String] -> Locatable UnsizedBitsExpr -> ParserMonad (Int, [Bit], BitsExpr)
instEncDim' ts vs e = case locatableValue e of
  (UnsizedConstBitsExpr bs)     -> return (length . locatableValue $ bs, locatableValue bs, ConstBitsExpr [])
  (UnsizedEncBitsExpr v)        ->
    case filter ((== locatableValue v) . fst) $ zip (locatableValue vs) ts of
      []       -> throwLocalError v $ "No such variable " ++ locatableValue v
      [(_, t)] -> (\n -> (n, [], EncBitsExpr n (locatableValue v))) <$> getEncType (pure t)
      _        -> throwLocalError vs $ "Two variables with the same name " ++ locatableValue v
  (UnsizedConcatBitsExpr e1 e2) -> do
    (n1, bs1, e1') <- instEncDim' ts vs e1
    (n2, bs2, e2') <- instEncDim' ts vs e2
    case e1' of
      ConstBitsExpr [] -> return (n1 + n2, bs1 ++ bs2, e2')
      _                -> return (n1 + n2, bs1, ConcatBitsExpr e1' (ConcatBitsExpr (ConstBitsExpr bs2) e2'))
  (UnsizedAndBitsExpr e1 e2)    -> do
    (n1, bs1, e1') <- instEncDim' ts vs e1
    (n2, bs2, e2') <- instEncDim' ts vs e2
    unless (n1 == n2) . throwLocalError e $ "Mismatched dimensions of bitwise and: Bits " ++ show n1 ++ " and Bits " ++ show n2
    return (n1, [], AndBitsExpr (ConcatBitsExpr (ConstBitsExpr bs1) e1') (ConcatBitsExpr (ConstBitsExpr bs2) e2'))
  (UnsizedOrBitsExpr e1 e2)     -> do
    (n1, bs1, e1') <- instEncDim' ts vs e1
    (n2, bs2, e2') <- instEncDim' ts vs e2
    unless (n1 == n2) . throwLocalError e $ "Mismatched dimensions of bitwise or: Bits " ++ show n1 ++ " and Bits " ++ show n2
    return (n1, [], OrBitsExpr (ConcatBitsExpr (ConstBitsExpr bs1) e1') (ConcatBitsExpr (ConstBitsExpr bs2) e2'))
  (UnsizedXorBitsExpr e1 e2)    -> do
    (n1, bs1, e1') <- instEncDim' ts vs e1
    (n2, bs2, e2') <- instEncDim' ts vs e2
    unless (n1 == n2) . throwLocalError e $ "Mismatched dimensions of bitwise exclusive or: Bits " ++ show n1 ++ " and Bits " ++ show n2
    return (n1, [], XorBitsExpr (ConcatBitsExpr (ConstBitsExpr bs1) e1') (ConcatBitsExpr (ConstBitsExpr bs2) e2'))

-- | PROBABLY UNNEEDED ALONG WITH `UnsizedBitsExpr'
instEncDim :: [Type] -> Locatable [String] -> Locatable UnsizedBitsExpr -> ParserMonad (Locatable Int, Locatable ([Bit], BitsExpr))
instEncDim ts vs e = do
  (n, bs, e') <- instEncDim' ts vs e
  when (null bs) $ throwLocalError e "Instruction encodings must have a constant prefix"
  return (n <$ e, (bs, e') <$ e)

runParser' :: ParserMonad a -> String -> Either (String, Maybe (AlexPosn, AlexPosn)) a
runParser' m = (fst <$>) . runStateT m . initialParserState

printErrors :: String -> Either (String, Maybe (AlexPosn, AlexPosn)) a -> Either String a
printErrors _ (Right a)           = Right a
printErrors _ (Left (e, Nothing)) = Left $ e ++ "\n"
printErrors s (Left (e, Just (AlexPosn a1 l1 c1, AlexPosn a2 l2 c2)))
  | l1 == l2 = Left $
      let (line1, line2) =  (reverse . takeWhile (/= '\n') . reverse *** takeWhile (/= '\n')) . splitAt a1 $ s in
      prettyPosn (Just (AlexPosn a1 l1 c1, AlexPosn a2 l2 c2)) ++ ": " ++ e ++ "\n" ++ line1 ++ line2 ++ "\n" ++ replicate (c1 - 1) ' ' ++ replicate (c2 - c1) '^' ++ "\n"
  | otherwise = Left $
      prettyPosn (Just (AlexPosn a1 l1 c1, AlexPosn a2 l2 c2)) ++ ": " ++ e ++ "\nError spanning mulitple lines, I don't yet know how to display that!!!"

-- | Run the given parser on the given input string and either return a nicely formatted error or the output of the parser
runParser :: ParserMonad a -> String -> Either String a
runParser m s = printErrors s $ runParser' m s
