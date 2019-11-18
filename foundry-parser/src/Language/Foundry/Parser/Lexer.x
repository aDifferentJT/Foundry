{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

{-|
Module      : Language.Foundry.Parser.Lexer
Description : The lexer
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.Foundry.Parser.Lexer
  ( Token(..)
  , readToken
  ) where

import ClassyPrelude

import Data.Bit (Bit)
import Language.Foundry.Parser.AlexPosn (AlexPosn, Locatable(Locatable))
import Language.Foundry.Parser.Monad

import qualified Control.Monad.Trans.State as State
import qualified Data.Text as Text
import Text.Read (read)
}

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$lower = [a-z]     -- lower case characters
$upper = [A-Z]     -- upper case characters

tokens :-
  ($white # \n)+;
  \\\n;
  \n                          { wrapPlainToken Semi }
  ";"                         { wrapPlainToken Semi }
  "--".*\n                    { wrapPlainToken Semi }
  leds                        { wrapPlainToken LedsTok }
  led                         { wrapPlainToken LedTok }
  ":"                         { wrapPlainToken Colon }
  "="                         { wrapPlainToken Equals }
  "+"                         { wrapPlainToken Plus }
  "-"                         { wrapPlainToken Minus }
  "*"                         { wrapPlainToken Times }
  "/"                         { wrapPlainToken Slash }
  "&"                         { wrapPlainToken And }
  "|"                         { wrapPlainToken Or }
  "^"                         { wrapPlainToken Xor }
  "++"                        { wrapPlainToken Concat }
  "=="                        { wrapPlainToken Equality }
  "!="                        { wrapPlainToken Inequality }
  "&&"                        { wrapPlainToken LogicalAnd }
  "||"                        { wrapPlainToken LogicalOr }
  "?"                         { wrapPlainToken Question }
  "<-"                        { wrapPlainToken LeftArrow }
  "<"                         { wrapPlainToken OpenAngle }
  ">"                         { wrapPlainToken CloseAngle }
  "{"                         { wrapPlainToken OpenCurly }
  "}"                         { wrapPlainToken CloseCurly }
  "("                         { wrapPlainToken OpenParen }
  ")"                         { wrapPlainToken CloseParen }
  "["                         { wrapPlainToken OpenSquare }
  "]"                         { wrapPlainToken CloseSquare }
  0b[01]+                     { wrapFuncToken $ Bits . ((map (read . (:[])) . unpack . Text.drop 2) <$>) }
  $digit+                     { wrapFuncToken $ Int . (read . unpack <$>) }
  $lower [$alpha $digit \_]*  { wrapFuncToken VarTok }
  Reg                         { wrapPlainToken RegTok }
  Bits                        { wrapPlainToken BitsTok }
  Int                         { wrapPlainToken IntTok }
  Inst                        { wrapPlainToken InstTok }
  Button                      { wrapPlainToken ButtonTok }
  RAM                         { wrapPlainToken RAMTok }
  $upper [$alpha $digit \_]*  { \str ps -> throwLocalErrorAt' (Just ps) ("Unrecognised type name: " ++ str) }

{
-- | The tokens to lex
data Token
  = Semi                    -- ^ @;@
  | LedsTok                 -- ^ @leds@
  | LedTok                  -- ^ @led@
  | Colon                   -- ^ @:@
  | Minus                   -- ^ @-@
  | Equals                  -- ^ @=@
  | Plus                    -- ^ @+@
  | Times                   -- ^ @*@
  | Slash                   -- ^ @/@
  | And                     -- ^ @&@
  | Or                      -- ^ @|@
  | Xor                     -- ^ @^@
  | Concat                  -- ^ @++@
  | Equality                -- ^ @==@
  | Inequality              -- ^ @!=@
  | LogicalAnd              -- ^ @&&@
  | LogicalOr               -- ^ @||@
  | Question                -- ^ @?@
  | LeftArrow               -- ^ @<-@
  | OpenAngle               -- ^ @<@
  | CloseAngle              -- ^ @>@
  | OpenCurly               -- ^ @{@
  | CloseCurly              -- ^ @}@
  | OpenParen               -- ^ @(@
  | CloseParen              -- ^ @)@
  | OpenSquare              -- ^ @[@
  | CloseSquare             -- ^ @]@
  | Bits (Locatable [Bit])  -- ^ A binary literal
  | Int (Locatable Int)     -- ^ An integer
  | VarTok (Locatable Text) -- ^ An identifier starting with a lower case letter
  | RegTok                  -- ^ @Reg@
  | BitsTok                 -- ^ @Bits@
  | IntTok                  -- ^ @Int@
  | InstTok                 -- ^ @Inst@
  | ButtonTok               -- ^ @Button@
  | RAMTok                  -- ^ @RAM@
  | EOF                     -- ^ The end of the file
  deriving (Show)

wrapFuncToken :: (Locatable b -> a) -> b -> (AlexPosn, AlexPosn) -> ParserMonad (Locatable a)
wrapFuncToken f s ps = return $ Locatable (f . Locatable s . Just $ ps) (Just ps)

wrapPlainToken :: a -> b -> (AlexPosn, AlexPosn) -> ParserMonad (Locatable a)
wrapPlainToken = wrapFuncToken . const

-- | Read a `Token' from the input
readToken :: ParserMonad (Locatable Token)
readToken = do
  ParserState{..} <- State.get
  case alexScan stateInput 0 of
    AlexEOF                ->
      return . Locatable EOF . Just $ (charPos stateInput, charPos stateInput)
    AlexError input'       -> do
      State.put ParserState{ stateInput = input', .. }
      throwLocalErrorAt' (Just (charPos stateInput, alexMove (charPos input') (Text.head . str $ input')))
        $ "Could not lex token: " ++ tshow (Text.head . str $ input')
    AlexSkip input' _      -> do
      State.put ParserState{ stateInput = input', .. }
      readToken
    AlexToken input' len t -> do
      State.put ParserState{ stateInput = input', .. }
      (t . Text.take len . str $ stateInput) (charPos stateInput, charPos input')
}
