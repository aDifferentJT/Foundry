{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE RecordWildCards #-}

module Parser.Lexer
  ( Token(..)
  , readToken
  ) where

import Parser.LexerMonad

import Utils (Bit(..))

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
  buttons                     { const . return $ Buttons }
  memory                      { const . return $ MemoryTok }
  \:                          { const . return $ Colon }
  \-                          { const . return $ Hyphen }
  \=                          { const . return $ Equals }
  \+                          { const . return $ Plus }
  \*                          { const . return $ Times }
  \/                          { const . return $ Slash }
  \&                          { const . return $ And }
  \|                          { const . return $ Or }
  \^                          { const . return $ Xor }
  \+\+                        { const . return $ Concat }
  \=\=                        { const . return $ Equality }
  \&\&                        { const . return $ LogicalAnd }
  \|\|                        { const . return $ LogicalOr }
  \?                          { const . return $ Question }
  \<\-                        { const . return $ LeftArrow }
  \<                          { const . return $ OpenAngle }
  \>                          { const . return $ CloseAngle }
  \{                          { const . return $ OpenCurly }
  \}                          { const . return $ CloseCurly }
  \(                          { const . return $ OpenParen }
  \)                          { const . return $ CloseParen }
  \[                          { const . return $ OpenSquare }
  \]                          { const . return $ CloseSquare }
  0b[01]+                     { return . Bits . map (read . (:[])) . drop 2 }
  $digit+                     { return . Int . read }
  $lower [$alpha $digit \_]*  { return . VarTok }
  Reg                         { const . return $ RegTok }
  Bits                        { const . return $ BitsTok }
  Int                         { const . return $ IntTok }
  Inst                        { const . return $ InstTok }
  Button                      { const . return $ ButtonTok }
  RAM                         { const . return $ RAMTok }
  $upper [$alpha $digit \_]*  { throwLocalError 0 . ("Unrecognised type name: " ++) }

{
data Token
  = Registers
  | Instructions
  | Buttons
  | MemoryTok
  | Colon
  | Hyphen
  | Equals
  | Plus
  | Times
  | Slash
  | And
  | Or
  | Xor
  | Concat
  | Equality
  | LogicalAnd
  | LogicalOr
  | Question
  | LeftArrow
  | OpenAngle
  | CloseAngle
  | OpenCurly
  | CloseCurly
  | OpenParen
  | CloseParen
  | OpenSquare
  | CloseSquare
  | Bits [Bit]
  | Int Int
  | VarTok String
  | RegTok
  | BitsTok
  | IntTok
  | InstTok
  | ButtonTok
  | RAMTok
  | EOF
  deriving (Eq,Show)

readToken :: LexerMonad Token
readToken = do
  LexerState{..} <- State.get
  case alexScan stateInput 0 of
    AlexEOF                -> return EOF
    AlexError input'       -> State.put LexerState{ stateInput = input', .. } >> throwLocalError 0 "Could not lex token"
    AlexSkip input' _      -> State.put LexerState{ stateInput = input', .. } >> readToken
    AlexToken input' len t -> State.put LexerState{ stateInput = input' { tokPos = take 10 $ charPos stateInput : tokPos input' }, .. } >> (t . take len . str $ stateInput)
}

