{
module Lexer
  ( Token(..)
  , alexScanTokens
  ) where

import Utils(Bit(..))
}

%wrapper "basic"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$lower = [a-z]     -- lower case characters
$upper = [A-Z]     -- upper case characters

tokens :-

  $white+    ;
  "--".*     ;
  registers                      { const Registers }
  instructions                   { const Instructions }
  \:                             { const Colon }
  \-                             { const Hyphen }
  \=                             { const Equals }
  \+                             { const Plus }
  \*                             { const Times }
  \/                             { const Slash }
  \+\+                           { const Concat }
  \<\-                           { const LeftArrow }
  \<                             { const OpenAngle }
  \>                             { const CloseAngle }
  \{                             { const OpenCurly }
  \}                             { const CloseCurly }
  \(                             { const OpenParen }
  \)                             { const CloseParen }
  0b[01]+                        { Bits . map readBit . drop 2 }
  $digit+                        { Int . read }
  $lower [$alpha $digit \_ \']*  { VarTok }
  $upper [$alpha $digit \_ \']*  { TypeTok }

{
readBit :: Char -> Bit
readBit '0' = Zero
readBit '1' = One

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
  | TypeTok String
  deriving (Eq,Show)
}

