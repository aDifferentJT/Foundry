{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TupleSections, RecordWildCards #-}

module Parser.LexerMonad
  ( AlexInput(..)
  , alexGetByte
  , alexInputPrevChar
  , Defn(..)
  , LexerState(..)
  , LexerMonad
  , throwLocalError
  , throwGlobalError
  , defineLocalVar
  , defineReg
  , defineInst
  , defineButton
  , defineMemory
  , checkRegDefined
  , checkInstDefined
  , checkButtonDefined
  , checkMemoryDefined
  , getIdentifierDefn
  , clearLocalVars
  , runLexer
  ) where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Control.Monad.Trans.State as State

import Data.Char (ord)
import qualified Data.Bits
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.Word (Word8)

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
                  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc
                        , [
                        ])

   | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , [0x80 + oc Data.Bits..&. 0x3f
                        ])

   | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])
   | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])

type Byte = Word8

-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPosn !Int !Int !Int
  deriving (Eq, Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPosn 0 1 1

alex_tab_size :: Int
alex_tab_size = 8

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPosn a l c) '\t' = AlexPosn (a + 1)  l       (c + alex_tab_size - ((c - 1) `mod` alex_tab_size))
alexMove (AlexPosn a l _) '\n' = AlexPosn (a + 1) (l + 1)   1
alexMove (AlexPosn a l c) _    = AlexPosn (a + 1)  l       (c + 1)

data AlexInput = AlexInput
  { tokPos   :: [AlexPosn]  -- 5 most recent token positions
  , charPos  :: AlexPosn    -- position of the char
  , charPrev :: Char        -- previous char
  , pending  :: [Byte]      -- pending bytes on current char
  , str      :: String      -- current input string
  }

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes input = input { pending = [] }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = charPrev

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte AlexInput{..} = case (pending, str) of
  ((b:bs), _  ) -> Just (b,AlexInput{ pending = bs, .. })
  ([]    , [] ) -> Nothing
  ([]    , c:s) ->
    let p = alexMove charPos c in
    let (b, bs) = utf8Encode' c in
    p `seq`  Just (b, AlexInput tokPos p c bs s)

data Defn
  = RegDefn
  | InstDefn
  | ButtonDefn
  | MemoryDefn

data LexerState = LexerState
  { stateInput        :: AlexInput
  , stateLocalVars    :: Set.Set String
  , stateGlobalIdents :: Map.Map String Defn
  }

type LexerMonad a = StateT LexerState (Either (String, Maybe AlexPosn)) a

previousPos :: Int -> LexerMonad AlexPosn
previousPos n = (!! n) . tokPos . stateInput <$> State.get

throwLocalError :: Int -> String -> LexerMonad a
throwLocalError n s = previousPos n >>= throwError . (s,) . Just

throwGlobalError :: MonadError (String, Maybe AlexPosn) m => String -> m a
throwGlobalError = throwError . (, Nothing)

defineLocalVar :: String -> LexerMonad ()
defineLocalVar var = do
  LexerState{..} <- State.get
  when (Set.member var stateLocalVars) . throwLocalError 2 $ "Variable " ++ var ++ " redefined"
  when (Map.member var stateGlobalIdents) . throwLocalError 2 $ "Variable " ++ var ++ " has the same name as a previously defined identifier"
  State.put $ LexerState{ stateLocalVars = Set.insert var stateLocalVars, .. }

defineReg :: String -> LexerMonad ()
defineReg var = do
  LexerState{..} <- State.get
  case Map.lookup var stateGlobalIdents of
    Just RegDefn    -> throwLocalError 2 $ "Register " ++ var ++ " has the same name as a previously defined register"
    Just InstDefn   -> throwLocalError 2 $ "Register " ++ var ++ " has the same name as a previously defined instruction"
    Just ButtonDefn -> throwLocalError 2 $ "Register " ++ var ++ " has the same name as a previously defined button"
    Just MemoryDefn -> throwLocalError 2 $ "Register " ++ var ++ " has the same name as a previously defined memory"
    Nothing     -> State.put $ LexerState{ stateGlobalIdents = Map.insert var RegDefn stateGlobalIdents, .. }

defineInst :: String -> LexerMonad ()
defineInst var = do
  LexerState{..} <- State.get
  case Map.lookup var stateGlobalIdents of
    Just RegDefn    -> throwLocalError 2 $ "Instruction " ++ var ++ " has the same name as a previously defined register"
    Just InstDefn   -> throwLocalError 2 $ "Instruction " ++ var ++ " has the same name as a previously defined instruction"
    Just ButtonDefn -> throwLocalError 2 $ "Instruction " ++ var ++ " has the same name as a previously defined button"
    Just MemoryDefn -> throwLocalError 2 $ "Instruction " ++ var ++ " has the same name as a previously defined memory"
    Nothing     -> State.put $ LexerState{ stateGlobalIdents = Map.insert var InstDefn stateGlobalIdents, .. }

defineButton :: String -> LexerMonad ()
defineButton var = do
  LexerState{..} <- State.get
  case Map.lookup var stateGlobalIdents of
    Just RegDefn    -> throwLocalError 2 $ "Button " ++ var ++ " has the same name as a previously defined register"
    Just InstDefn   -> throwLocalError 2 $ "Button " ++ var ++ " has the same name as a previously defined instruction"
    Just ButtonDefn -> throwLocalError 2 $ "Button " ++ var ++ " has the same name as a previously defined button"
    Just MemoryDefn -> throwLocalError 2 $ "Button " ++ var ++ " has the same name as a previously defined memory"
    Nothing     -> State.put $ LexerState{ stateGlobalIdents = Map.insert var ButtonDefn stateGlobalIdents, .. }

defineMemory :: String -> LexerMonad ()
defineMemory var = do
  LexerState{..} <- State.get
  case Map.lookup var stateGlobalIdents of
    Just RegDefn    -> throwLocalError 2 $ "Memory " ++ var ++ " has the same name as a previously defined register"
    Just InstDefn   -> throwLocalError 2 $ "Memory " ++ var ++ " has the same name as a previously defined instruction"
    Just ButtonDefn -> throwLocalError 2 $ "Memory " ++ var ++ " has the same name as a previously defined button"
    Just MemoryDefn -> throwLocalError 2 $ "Memory " ++ var ++ " has the same name as a previously defined memory"
    Nothing     -> State.put $ LexerState{ stateGlobalIdents = Map.insert var MemoryDefn stateGlobalIdents, .. }

checkRegDefined :: String -> LexerMonad ()
checkRegDefined var = do
  LexerState{..} <- State.get
  if Set.member var stateLocalVars
  then return ()
  else case Map.lookup var stateGlobalIdents of
    Just RegDefn    -> return ()
    Just InstDefn   -> throwLocalError 1 $ var ++ " is an instruction, expected a register"
    Just ButtonDefn -> throwLocalError 1 $ var ++ " is a button, expected a register"
    Just MemoryDefn -> throwLocalError 1 $ var ++ " is a memory, expected a register"
    Nothing     -> throwLocalError 1 $ "Register/variable " ++ var ++ " not defined"

checkInstDefined :: String -> LexerMonad ()
checkInstDefined var = do
  LexerState{..} <- State.get
  if Set.member var stateLocalVars
  then throwLocalError 1 $ var ++ " is a local variable, expected an instruction"
  else case Map.lookup var stateGlobalIdents of
    Just RegDefn    -> throwLocalError 1 $ var ++ " is a register, expected an instruction"
    Just InstDefn   -> return ()
    Just ButtonDefn -> throwLocalError 1 $ var ++ " is a button, expected an instruction"
    Just MemoryDefn -> throwLocalError 1 $ var ++ " is a memory, expected an instruction"
    Nothing     -> throwLocalError 1 $ "Instruction " ++ var ++ " not defined"

checkButtonDefined :: String -> LexerMonad ()
checkButtonDefined var = do
  LexerState{..} <- State.get
  if Set.member var stateLocalVars
  then throwLocalError 1 $ var ++ " is a local variable, expected a button"
  else case Map.lookup var stateGlobalIdents of
    Just RegDefn    -> throwLocalError 1 $ var ++ " is a register, expected a button"
    Just InstDefn   -> throwLocalError 1 $ var ++ " is an instruction, expected a button"
    Just ButtonDefn -> return ()
    Just MemoryDefn -> throwLocalError 1 $ var ++ " is a memory, expected a button"
    Nothing     -> throwLocalError 1 $ "Button " ++ var ++ " not defined"

checkMemoryDefined :: String -> LexerMonad ()
checkMemoryDefined var = do
  LexerState{..} <- State.get
  if Set.member var stateLocalVars
  then throwLocalError 1 $ var ++ " is a local variable, expected a memory"
  else case Map.lookup var stateGlobalIdents of
    Just RegDefn    -> throwLocalError 1 $ var ++ " is a register, expected a memory"
    Just InstDefn   -> throwLocalError 1 $ var ++ " is an instruction, expected a memory"
    Just ButtonDefn -> throwLocalError 1 $ var ++ " is a button, expected a memory"
    Just MemoryDefn -> return ()
    Nothing     -> throwLocalError 1 $ "Memory " ++ var ++ " not defined"

getIdentifierDefn :: String -> LexerMonad Defn
getIdentifierDefn var = do
  LexerState{..} <- State.get
  case Map.lookup var stateGlobalIdents of
    Nothing -> throwLocalError 1 $ "Identifier " ++ var ++ " not defined"
    Just d  -> return d

clearLocalVars :: LexerMonad ()
clearLocalVars = do
  LexerState{..} <- State.get
  State.put $ LexerState{ stateLocalVars = Set.empty, .. }

runLexer' :: LexerMonad a -> String -> Either (String, Maybe AlexPosn) a
runLexer' m s = fst <$> runStateT m (LexerState (AlexInput [alexStartPos] alexStartPos '\n' [] s) Set.empty Map.empty)

printErrors :: String -> Either (String, Maybe AlexPosn) a -> Either String a
printErrors _ (Right a)                          = Right a
printErrors _ (Left (e, Nothing))                = Left $ e ++ "\n"
printErrors s (Left (e, Just (AlexPosn a l c) )) = Left $ prettyPos ++ ": " ++ e ++ "\n" ++ line1 ++ line2 ++ "\n" ++ replicate (c - 1) ' ' ++ "^\n"
  where prettyPos :: String
        prettyPos = "Line " ++ show l ++ " Column " ++ show c
        (line1, line2) =  (reverse . takeWhile (/= '\n') . reverse *** takeWhile (/= '\n')) . splitAt a $ s

runLexer :: LexerMonad a -> String -> Either String a
runLexer m s = printErrors s $ runLexer' m s

