{
{-# LANGUAGE RecordWildCards #-}

module Parser.Lexer
  ( Token(..)
  , readToken
  ) where

import Parser.LexerMonad

import Utils (Bit(..))

import Control.Monad.Except
import qualified Control.Monad.Trans.State as State
}

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$lower = [a-z]     -- lower case characters
$upper = [A-Z]     -- upper case characters

tokens :-

  $white+    ;
  "--".*     ;
  registers                   { const . return $ Registers }
  instructions                { const . return $ Instructions }
  \:                          { const . return $ Colon }
  \-                          { const . return $ Hyphen }
  \=                          { const . return $ Equals }
  \+                          { const . return $ Plus }
  \*                          { const . return $ Times }
  \/                          { const . return $ Slash }
  \+\+                        { const . return $ Concat }
  \<\-                        { const . return $ LeftArrow }
  \<                          { const . return $ OpenAngle }
  \>                          { const . return $ CloseAngle }
  \{                          { const . return $ OpenCurly }
  \}                          { const . return $ CloseCurly }
  \(                          { const . return $ OpenParen }
  \)                          { const . return $ CloseParen }
  0b[01]+                     { return . Bits . map (read . (:[])) . drop 2 }
  $digit+                     { return . Int . read }
  $lower [$alpha $digit \_]*  { return . VarTok }
  Reg                         { const . return $ RegTok }
  Bits                        { const . return $ BitsTok }
  Int                         { const . return $ IntTok }
  Inst                        { const . return $ InstTok }
  $upper [$alpha $digit \_]*  { throwLocalError 0 . ("Unrecognised type name: " ++) }

{
data Token
  = Registers
  | Instructions
  | Colon
  | Hyphen
  | Equals
  | Plus
  | Times
  | Slash
  | Concat
  | LeftArrow
  | OpenAngle
  | CloseAngle
  | OpenCurly
  | CloseCurly
  | OpenParen
  | CloseParen
  | Bits [Bit]
  | Int Int
  | VarTok String
  | RegTok
  | BitsTok
  | IntTok
  | InstTok
  | EOF
  deriving (Eq,Show)

readToken :: LexerMonad Token
readToken = do
  LexerState input vars <- State.get
  case alexScan input 0 of
    AlexEOF                -> return EOF
    AlexError input'       -> State.put (LexerState input' vars) >> throwLocalError 0 "Could not lex token"
    AlexSkip input' _      -> State.put (LexerState input' vars) >> readToken
    AlexToken input' len t -> State.put (LexerState (input' { tokPos = take 5 $ charPos input : tokPos input' }) vars) >> (t . take len . str $ input)
}

