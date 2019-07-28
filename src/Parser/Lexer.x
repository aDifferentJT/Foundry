{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE RecordWildCards #-}

module Parser.Lexer
  ( Token(..)
  , readToken
  ) where

import Parser.AlexPosn (AlexPosn, Locatable(Locatable))
import Parser.Monad

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
  registers                   { wrapPlainToken Registers }
  instructions                { wrapPlainToken Instructions }
  buttons                     { wrapPlainToken Buttons }
  memory                      { wrapPlainToken MemoryTok }
  \:                          { wrapPlainToken Colon }
  \-                          { wrapPlainToken Hyphen }
  \=                          { wrapPlainToken Equals }
  \+                          { wrapPlainToken Plus }
  \*                          { wrapPlainToken Times }
  \/                          { wrapPlainToken Slash }
  \&                          { wrapPlainToken And }
  \|                          { wrapPlainToken Or }
  \^                          { wrapPlainToken Xor }
  \+\+                        { wrapPlainToken Concat }
  \=\=                        { wrapPlainToken Equality }
  \&\&                        { wrapPlainToken LogicalAnd }
  \|\|                        { wrapPlainToken LogicalOr }
  \?                          { wrapPlainToken Question }
  \<\-                        { wrapPlainToken LeftArrow }
  \<                          { wrapPlainToken OpenAngle }
  \>                          { wrapPlainToken CloseAngle }
  \{                          { wrapPlainToken OpenCurly }
  \}                          { wrapPlainToken CloseCurly }
  \(                          { wrapPlainToken OpenParen }
  \)                          { wrapPlainToken CloseParen }
  \[                          { wrapPlainToken OpenSquare }
  \]                          { wrapPlainToken CloseSquare }
  0b[01]+                     { wrapFuncToken $ Bits . ((map (read . (:[])) . drop 2) <$>) }
  $digit+                     { wrapFuncToken $ Int . (read <$>) }
  $lower [$alpha $digit \_]*  { wrapFuncToken VarTok }
  Reg                         { wrapPlainToken RegTok }
  Bits                        { wrapPlainToken BitsTok }
  Int                         { wrapPlainToken IntTok }
  Inst                        { wrapPlainToken InstTok }
  Button                      { wrapPlainToken ButtonTok }
  RAM                         { wrapPlainToken RAMTok }
  $upper [$alpha $digit \_]*  { \str ps -> throwLocalErrorAt ps ("Unrecognised type name: " ++ str) }

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
  | Bits (Locatable [Bit])
  | Int (Locatable Int)
  | VarTok (Locatable String)
  | RegTok
  | BitsTok
  | IntTok
  | InstTok
  | ButtonTok
  | RAMTok
  | EOF
  deriving (Show)

wrapFuncToken :: (Locatable b -> a) -> b -> (AlexPosn, AlexPosn) -> ParserMonad (Locatable a)
wrapFuncToken f s ps = return $ Locatable (f . Locatable s . Just $ ps) (Just ps)

wrapPlainToken :: a -> b -> (AlexPosn, AlexPosn) -> ParserMonad (Locatable a)
wrapPlainToken = wrapFuncToken . const

readToken :: ParserMonad (Locatable Token)
readToken = do
  ParserState{..} <- State.get
  case alexScan stateInput 0 of
    AlexEOF                ->
      return . Locatable EOF . Just $ (charPos stateInput, charPos stateInput)
    AlexError input'       -> do
      State.put ParserState{ stateInput = input', .. }
      throwLocalErrorAt (charPos stateInput, alexMove (charPos input') (head . str $ input')) "Could not lex token"
    AlexSkip input' _      -> do
      State.put ParserState{ stateInput = input', .. }
      readToken
    AlexToken input' len t -> do
      State.put ParserState{ stateInput = input', .. }
      (t . take len . str $ stateInput) (charPos stateInput, charPos input')
}
