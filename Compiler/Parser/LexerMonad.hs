{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TupleSections, RecordWildCards #-}

module Parser.LexerMonad
  ( AlexInput(..)
  , alexGetByte
  , alexInputPrevChar
  , LexerState(..)
  , LexerMonad
  , throwLocalError
  , throwGlobalError
  , defineVar
  , checkDefined
  , clearDefined
  , runLexer
  ) where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Control.Monad.Trans.State as State

import Data.Char (ord)
import qualified Data.Bits
import qualified Data.Set as Set
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

data LexerState = LexerState AlexInput (Set.Set String)

type LexerMonad a = StateT LexerState (Either (String, Maybe AlexPosn)) a

previousPos :: Int -> LexerMonad AlexPosn
previousPos n = (\(LexerState AlexInput{..} _) -> tokPos !! n) <$> State.get

throwLocalError :: Int -> String -> LexerMonad a
throwLocalError n s = previousPos n >>= throwError . (s,) . Just

throwGlobalError :: MonadError (String, Maybe AlexPosn) m => String -> m a
throwGlobalError = throwError . (, Nothing)

defineVar :: String -> LexerMonad ()
defineVar var = do
  LexerState input vars <- State.get
  when (Set.member var vars) . throwLocalError 2 $ "Variable " ++ var ++ " redefined"
  let vars' = Set.insert var vars
  State.put $ LexerState input vars'

checkDefined :: String -> LexerMonad ()
checkDefined var = do
  LexerState input vars <- State.get
  unless (Set.member var vars) . throwLocalError 1 $ "Variable " ++ var ++ " not defined"

clearDefined :: LexerMonad ()
clearDefined = do
  LexerState input _ <- State.get
  State.put $ LexerState input Set.empty

runLexer' :: LexerMonad a -> String -> Either (String, Maybe AlexPosn) a
runLexer' m s = fst <$> runStateT m (LexerState (AlexInput [alexStartPos] alexStartPos '\n' [] s) Set.empty)

printErrors :: String -> Either (String, Maybe AlexPosn) a -> Either String a
printErrors _ (Right a)           = Right a
printErrors _ (Left (e, Nothing)) = Left e
printErrors s (Left (e, Just (AlexPosn a l c) )) = Left $ prettyPos ++ ": " ++ e ++ "\n" ++ line1 ++ line2 ++ "\n" ++ replicate (c - 1) ' ' ++ "^\n"
  where prettyPos :: String
        prettyPos = "Line " ++ show l ++ " Column " ++ show c
        (line1, line2) =  (reverse . takeWhile (/= '\n') . reverse *** takeWhile (/= '\n')) . splitAt a $ s

runLexer :: LexerMonad a -> String -> Either String a
runLexer m s = printErrors s $ runLexer' m s

