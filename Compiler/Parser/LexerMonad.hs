{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TupleSections, RecordWildCards #-}

module Parser.LexerMonad
  ( AlexInput(..)
  , ignorePendingBytes
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
  , runLexer
  ) where

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
import Control.Monad.Except (MonadError, throwError)
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
    let (b, bs) = utf8Encode c in
    p `seq`  Just (b, AlexInput tokPos p c bs s)

data Defn
  = RegDefn Int
  | InstDefn [Type]
  | ButtonDefn
  | MemoryDefn Int Int

data LexerState = LexerState
  { stateInput        :: AlexInput
  , stateLocalVars    :: Set.Set String
  , stateGlobalIdents :: Map.Map String Defn
  , stateEncTypes     :: Map.Map Type Int
  }

type LexerMonad a = StateT LexerState (Either (String, Maybe AlexPosn)) a

previousPos :: Int -> LexerMonad AlexPosn
previousPos n = (!! n) . tokPos . stateInput <$> State.get

throwLocalError :: Int -> String -> LexerMonad a
throwLocalError n s = previousPos n >>= throwError . (s,) . Just

throwGlobalError :: MonadError (String, Maybe AlexPosn) m => String -> m a
throwGlobalError = throwError . (, Nothing)

defineLocalVar :: String -> LexerMonad String
defineLocalVar var = do
  LexerState{..} <- State.get
  when (Set.member var stateLocalVars) . throwLocalError 2 $ "Variable " ++ var ++ " redefined"
  when (Map.member var stateGlobalIdents) . throwLocalError 2 $ "Variable " ++ var ++ " has the same name as a previously defined identifier"
  State.put $ LexerState{ stateLocalVars = Set.insert var stateLocalVars, .. }
  return var

defineReg :: String -> Int -> LexerMonad RegType
defineReg var n = do
  LexerState{..} <- State.get
  case Map.lookup var stateGlobalIdents of
    Just (RegDefn _)      -> throwLocalError 2 $ "Register " ++ var ++ " has the same name as a previously defined register"
    Just (InstDefn _)     -> throwLocalError 2 $ "Register " ++ var ++ " has the same name as a previously defined instruction"
    Just  ButtonDefn      -> throwLocalError 2 $ "Register " ++ var ++ " has the same name as a previously defined button"
    Just (MemoryDefn _ _) -> throwLocalError 2 $ "Register " ++ var ++ " has the same name as a previously defined memory"
    Nothing     -> State.put $ LexerState{ stateGlobalIdents = Map.insert var (RegDefn n) stateGlobalIdents, .. }
  return $ RegType var n

defineInst :: String -> [Type] -> LexerMonad InstType
defineInst var ts = do
  LexerState{..} <- State.get
  case Map.lookup var stateGlobalIdents of
    Just (RegDefn _)      -> throwLocalError 2 $ "Instruction " ++ var ++ " has the same name as a previously defined register"
    Just (InstDefn _)     -> throwLocalError 2 $ "Instruction " ++ var ++ " has the same name as a previously defined instruction"
    Just  ButtonDefn      -> throwLocalError 2 $ "Instruction " ++ var ++ " has the same name as a previously defined button"
    Just (MemoryDefn _ _) -> throwLocalError 2 $ "Instruction " ++ var ++ " has the same name as a previously defined memory"
    Nothing     -> State.put $ LexerState{ stateGlobalIdents = Map.insert var (InstDefn ts) stateGlobalIdents, .. }
  return $ InstType var ts

defineButton :: String -> Int -> LexerMonad ButtonType
defineButton var n = do
  LexerState{..} <- State.get
  case Map.lookup var stateGlobalIdents of
    Just (RegDefn _)      -> throwLocalError 2 $ "Button " ++ var ++ " has the same name as a previously defined register"
    Just (InstDefn _)     -> throwLocalError 2 $ "Button " ++ var ++ " has the same name as a previously defined instruction"
    Just  ButtonDefn      -> throwLocalError 2 $ "Button " ++ var ++ " has the same name as a previously defined button"
    Just (MemoryDefn _ _) -> throwLocalError 2 $ "Button " ++ var ++ " has the same name as a previously defined memory"
    Nothing     -> State.put $ LexerState{ stateGlobalIdents = Map.insert var ButtonDefn stateGlobalIdents, .. }
  return $ ButtonType var n

defineMemory :: String -> Int -> Int -> LexerMonad Memory
defineMemory var dataWidth addressWidth = do
  LexerState{..} <- State.get
  case Map.lookup var stateGlobalIdents of
    Just (RegDefn _)      -> throwLocalError 2 $ "Memory " ++ var ++ " has the same name as a previously defined register"
    Just (InstDefn _)     -> throwLocalError 2 $ "Memory " ++ var ++ " has the same name as a previously defined instruction"
    Just  ButtonDefn      -> throwLocalError 2 $ "Memory " ++ var ++ " has the same name as a previously defined button"
    Just (MemoryDefn _ _) -> throwLocalError 2 $ "Memory " ++ var ++ " has the same name as a previously defined memory"
    Nothing     -> State.put $ LexerState{ stateGlobalIdents = Map.insert var (MemoryDefn dataWidth addressWidth) stateGlobalIdents, .. }
  return $ Memory var dataWidth addressWidth

isLocalVar :: String -> LexerMonad Bool
isLocalVar var = do
  LexerState{..} <- State.get
  return $ Set.member var stateLocalVars

checkLocalVar :: String -> LexerMonad ()
checkLocalVar var = do
  LexerState{..} <- State.get
  if Set.member var stateLocalVars
  then return ()
  else case Map.lookup var stateGlobalIdents of
    Just (RegDefn _)      -> throwLocalError 1 $ var ++ " is a register, expected a local variable"
    Just (InstDefn _)     -> throwLocalError 1 $ var ++ " is an instruction, expected a local variable"
    Just  ButtonDefn      -> throwLocalError 1 $ var ++ " is a button, expected a local variable"
    Just (MemoryDefn _ _) -> throwLocalError 1 $ var ++ " is a memory, expected a local variable"
    Nothing     -> throwLocalError 1 $ "Variable " ++ var ++ " not defined"

checkRegDefined :: String -> LexerMonad ()
checkRegDefined var = do
  LexerState{..} <- State.get
  if Set.member var stateLocalVars
  then throwLocalError 1 $ var ++ " is a local variable, expected a register"
  else case Map.lookup var stateGlobalIdents of
    Just (RegDefn _)      -> return ()
    Just (InstDefn _)     -> throwLocalError 1 $ var ++ " is an instruction, expected a register"
    Just  ButtonDefn      -> throwLocalError 1 $ var ++ " is a button, expected a register"
    Just (MemoryDefn _ _) -> throwLocalError 1 $ var ++ " is a memory, expected a register"
    Nothing     -> throwLocalError 1 $ "Register " ++ var ++ " not defined"

checkInstDefined :: String -> LexerMonad ()
checkInstDefined var = do
  LexerState{..} <- State.get
  if Set.member var stateLocalVars
  then throwLocalError 1 $ var ++ " is a local variable, expected an instruction"
  else case Map.lookup var stateGlobalIdents of
    Just (RegDefn _)      -> throwLocalError 1 $ var ++ " is a register, expected an instruction"
    Just (InstDefn _)     -> return ()
    Just  ButtonDefn      -> throwLocalError 1 $ var ++ " is a button, expected an instruction"
    Just (MemoryDefn _ _) -> throwLocalError 1 $ var ++ " is a memory, expected an instruction"
    Nothing     -> throwLocalError 1 $ "Instruction " ++ var ++ " not defined"

checkButtonDefined :: String -> LexerMonad ()
checkButtonDefined var = do
  LexerState{..} <- State.get
  if Set.member var stateLocalVars
  then throwLocalError 1 $ var ++ " is a local variable, expected a button"
  else case Map.lookup var stateGlobalIdents of
    Just (RegDefn _)      -> throwLocalError 1 $ var ++ " is a register, expected a button"
    Just (InstDefn _)     -> throwLocalError 1 $ var ++ " is an instruction, expected a button"
    Just  ButtonDefn      -> return ()
    Just (MemoryDefn _ _) -> throwLocalError 1 $ var ++ " is a memory, expected a button"
    Nothing     -> throwLocalError 1 $ "Button " ++ var ++ " not defined"

checkMemoryDefined :: String -> LexerMonad ()
checkMemoryDefined var = do
  LexerState{..} <- State.get
  if Set.member var stateLocalVars
  then throwLocalError 1 $ var ++ " is a local variable, expected a memory"
  else case Map.lookup var stateGlobalIdents of
    Just (RegDefn _)      -> throwLocalError 1 $ var ++ " is a register, expected a memory"
    Just (InstDefn _)     -> throwLocalError 1 $ var ++ " is an instruction, expected a memory"
    Just  ButtonDefn      -> throwLocalError 1 $ var ++ " is a button, expected a memory"
    Just (MemoryDefn _ _) -> return ()
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

defineEncType :: Type -> Int -> LexerMonad EncType
defineEncType t n = do
  LexerState{..} <- State.get
  when (Map.member t stateEncTypes) . throwLocalError 6 $ "Encoding of type " ++ show t ++ " redefined"
  State.put $ LexerState{ stateEncTypes = Map.insert t n stateEncTypes, .. }
  return $ EncType t n

getEncType :: Type -> LexerMonad Int
getEncType (BitsT n) = return n
getEncType (IntT n)  = return n
getEncType  t        = do
  LexerState{..} <- State.get
  case Map.lookup t stateEncTypes of
    Just n  -> return n
    Nothing -> throwLocalError 1 $ "Encoding of type " ++ show t ++ " not defined"

instEncDim' :: [Type] -> [String] -> UnsizedBitsExpr -> LexerMonad (Int, [Bit], BitsExpr)
instEncDim' _  _  (UnsizedConstBitsExpr bs)     = return (length bs, bs, ConstBitsExpr [])
instEncDim' ts vs (UnsizedEncBitsExpr v)        = do
  case filter ((== v) . fst) $ zip vs ts of
    []       -> throwLocalError 1 $ "No such variable " ++ v
    [(_, t)] -> (\n -> (n, [], EncBitsExpr n v)) <$> getEncType t
    _        -> throwLocalError 1 $ "Two variables with the same name " ++ v
instEncDim' ts vs (UnsizedConcatBitsExpr e1 e2) = do
  (n1, bs1, e1') <- instEncDim' ts vs e1
  (n2, bs2, e2') <- instEncDim' ts vs e2
  case e1' of
    ConstBitsExpr [] -> return (n1 + n2, bs1 ++ bs2, e2')
    _                -> return (n1 + n2, bs1, ConcatBitsExpr e1' (ConcatBitsExpr (ConstBitsExpr bs2) e2'))
instEncDim' ts vs (UnsizedAndBitsExpr e1 e2)    = do
  (n1, bs1, e1') <- instEncDim' ts vs e1
  (n2, bs2, e2') <- instEncDim' ts vs e2
  unless (n1 == n2) . throwLocalError 1 $ "Mismatched dimensions of bitwise and: Bits " ++ show n1 ++ " and Bits " ++ show n2
  return (n1, [], AndBitsExpr (ConcatBitsExpr (ConstBitsExpr bs1) e1') (ConcatBitsExpr (ConstBitsExpr bs2) e2'))
instEncDim' ts vs (UnsizedOrBitsExpr e1 e2)     = do
  (n1, bs1, e1') <- instEncDim' ts vs e1
  (n2, bs2, e2') <- instEncDim' ts vs e2
  unless (n1 == n2) . throwLocalError 1 $ "Mismatched dimensions of bitwise or: Bits " ++ show n1 ++ " and Bits " ++ show n2
  return (n1, [], OrBitsExpr (ConcatBitsExpr (ConstBitsExpr bs1) e1') (ConcatBitsExpr (ConstBitsExpr bs2) e2'))
instEncDim' ts vs (UnsizedXorBitsExpr e1 e2)    = do
  (n1, bs1, e1') <- instEncDim' ts vs e1
  (n2, bs2, e2') <- instEncDim' ts vs e2
  unless (n1 == n2) . throwLocalError 1 $ "Mismatched dimensions of bitwise exclusive or: Bits " ++ show n1 ++ " and Bits " ++ show n2
  return (n1, [], XorBitsExpr (ConcatBitsExpr (ConstBitsExpr bs1) e1') (ConcatBitsExpr (ConstBitsExpr bs2) e2'))

instEncDim :: [Type] -> [String] -> UnsizedBitsExpr -> LexerMonad (Int, [Bit], BitsExpr)
instEncDim ts vs e = do
  (n, bs, e') <- instEncDim' ts vs e
  when (null bs) $ throwLocalError 1 "Instruction encodings must have a constant prefix"
  return (n, bs, e')

runLexer' :: LexerMonad a -> String -> Either (String, Maybe AlexPosn) a
runLexer' m s = fst <$> runStateT m (LexerState (AlexInput [alexStartPos] alexStartPos '\n' [] s) Set.empty Map.empty Map.empty)

printErrors :: String -> Either (String, Maybe AlexPosn) a -> Either String a
printErrors _ (Right a)                          = Right a
printErrors _ (Left (e, Nothing))                = Left $ e ++ "\n"
printErrors s (Left (e, Just (AlexPosn a l c) )) = Left $ prettyPos ++ ": " ++ e ++ "\n" ++ line1 ++ line2 ++ "\n" ++ replicate (c - 1) ' ' ++ "^\n"
  where prettyPos :: String
        prettyPos = "Line " ++ show l ++ " Column " ++ show c
        (line1, line2) =  (reverse . takeWhile (/= '\n') . reverse *** takeWhile (/= '\n')) . splitAt a $ s

runLexer :: LexerMonad a -> String -> Either String a
runLexer m s = printErrors s $ runLexer' m s

