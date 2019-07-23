{
module Parser (parse) where

import Lexer
import LexerMonad

import Utils (Bit(..), zipBy, zip3By)
import AST

import Control.Monad.Except
import qualified Control.Monad.Trans.State as State
import Data.List(intercalate)
}

%name parse
%tokentype { Token }
%monad { LexerMonad }
%lexer { readToken >>= } { EOF }
%error { parseError }

%token
  registers     { Registers }
  instructions  { Instructions }
  ':'           { Colon }
  '-'           { Hyphen }
  '='           { Equals }
  '+'           { Plus }
  '*'           { Times }
  '/'           { Slash }
  '++'          { Concat }
  '<-'          { LeftArrow }
  '<'           { OpenAngle }
  '>'           { CloseAngle }
  '{'           { OpenCurly }
  '}'           { CloseCurly }
  '('           { OpenParen }
  ')'           { CloseParen }
  bits          { Bits $$ }
  int           { Int $$ }
  varTok        { VarTok $$ }
  regT          { RegTok }
  bitsT         { BitsTok }
  intT          { IntTok }
  instT         { InstTok }

%left '+' '-'
%left '*' '/'
%%

Proc              :: { UnsizedProc }
Proc              : RawProc                               {%
  case $1 of
    RawProc regs insts encTypes regEncs instEncs impls -> do
      regs' <- case regs of
        []  -> throwGlobalError "No register block"
        [x] -> return x
        _   -> throwGlobalError "More than one register block"
      regs'' <- case zipBy (\(RegType n _) -> n) (\(RegEnc n _) -> n) regs' regEncs of
        (_, (RegType n _):_, _) -> throwGlobalError $ "Register " ++ n ++ " has no encoding"
        (_, _, (RegEnc n _):_) -> throwGlobalError $ "Encoding given for unknown register " ++ n
        (xs, [], []) -> return $ [Reg n t e | (RegType n t, RegEnc _ e) <- xs]
      insts' <- case insts of
                 []  -> throwGlobalError "No instruction block"
                 [x] -> return x
                 _   -> throwGlobalError "More than one instruction block"
      let instName n vs = intercalate " " (n : ["<" ++ v ++ ">" | v <- vs])
      insts'' <- case zip3By (\(InstType n _) -> n) (\(InstImpl n _ _) -> n) (\(InstEnc n _ _) -> n) insts' impls instEncs of
        (_, (InstType n _, _):_, _, _, _, _, _) -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding"
        (_, _, (InstType n _, _):_, _, _, _, _) -> throwGlobalError $ "Instruction " ++ n ++ " has no implementation"
        (_, _, _, (InstImpl n vs _, _):_, _, _, _)    -> throwGlobalError $ "Implementation and encoding given for unknown instruction " ++ instName n vs
        (_, _, _, _, (InstType n _):_, _, _)    -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding or implementation"
        (_, _, _, _, _, (InstImpl n vs _):_, _)       -> throwGlobalError $ "Implementation given for unknown instruction " ++ instName n vs
        (_, _, _, _, _, _, (InstEnc n vs _):_)        -> throwGlobalError $ "Encoding given for unknown instruction " ++ instName n vs
        (xs, [], [], [], [], [], [])                  -> return $ [UnsizedInst n ts (vs1, rs) (vs2, e) | (InstType n ts, InstImpl _ vs1 rs, InstEnc _ vs2 e) <- xs]
      return $ UnsizedProc regs'' insts'' encTypes
}

RawProc           :: { RawProc }
RawProc           : {- empty -}                           { RawProc [] [] [] [] [] [] }
                  | RegTypes RawProc                      { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc ($1:regs) insts encTypes regEncs instEncs impls }
                  | InstTypes RawProc                     { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs ($1:insts) encTypes regEncs instEncs impls }
                  | EncType RawProc                       { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts ($1:encTypes) regEncs instEncs impls }
                  | RegEnc RawProc                        { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts encTypes ($1:regEncs) instEncs impls }
                  | InstEnc RawProc                       { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts encTypes regEncs ($1:instEncs) impls }
                  | InstImpl RawProc                      { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts encTypes regEncs instEncs ($1:impls) }

Var               :: { String }
Var               : varTok                                { $1 }

Type              :: { Type }
Type              : regT int                              { RegT $2 }
                  | bitsT int                             { BitsT $2 }
                  | intT int                              { IntT $2 }
                  | instT                                 { InstT }

Bits              :: { [Bit] }
Bits              : bits                                  { $1 }

RegType           :: { RegType }
RegType           : '-' Var ':' regT int                  { RegType $2 $5 }

RegTypeList       :: { [RegType] }
RegTypeList       : {- empty -}                           { [] }
                  | RegTypeList RegType                   { $2 : $1 }

RegTypes          :: { [RegType] }
RegTypes          : registers '{' RegTypeList '}'         { $3 }

EncType           :: { EncType }
EncType           : '<' Type '>' ':' bitsT int            { EncType $2 $6 }

RegEnc            :: { RegEnc }
RegEnc            : '<' Var '>' '=' Bits                  { RegEnc $2 $5 }

BitsExpr          :: { UnsizedBitsExpr }
BitsExpr          : Bits                                  { UnsizedConstBitsExpr $1 }
                  | '<' Var '>'                           { UnsizedEncBitsExpr $2 }
                  | BitsExpr '++' BitsExpr                { UnsizedConcatBitsExpr $1 $3 }

InstEnc           :: { InstEnc }
InstEnc           : '<' Var ArgList '>' '=' BitsExpr      {% clearDefined >> fmap (InstEnc $2 $3) (splitBitsExpr $6) }

TypeList          :: { [Type] }
TypeList          : {- empty -}                           { [] }
                  | TypeList '<' Type '>'                 { $3 : $1 }

InstType          :: { InstType }
InstType          : '-' Var TypeList                      { InstType $2 (reverse $3) }

InstTypeList      :: { [InstType] }
InstTypeList      : {- empty -}                           { [] }
                  | InstTypeList InstType                 { $2 : $1 }

InstTypes         :: { [InstType] }
InstTypes         : instructions '{' InstTypeList '}'     { $3 }

Expr              :: { Expr }
Expr              : '(' Expr ')'                          { $2 }
                  | Var                                   {% checkDefined $1 >> return (VarExpr $1) }
                  | int                                   { ConstExpr $1 }
                  | Expr '+' Expr                         { OpExpr Add $1 $3 }
                  | Expr '-' Expr                         { OpExpr Sub $1 $3 }
                  | Expr '*' Expr                         { OpExpr Mul $1 $3 }
                  | Expr '/' Expr                         { OpExpr Div $1 $3 }

ArgList           :: { [String] }
ArgList           : {- empty -}                           { [] }
                  | ArgList '<' Var '>'                   {% defineVar $3 >> return ($3 : $1) }

InstImplRule      :: { InstImplRule }
InstImplRule      : Var '<-' Expr                         {% checkDefined $1 >> return (InstImplRule $1 $3) }

InstImplRuleList  :: { [InstImplRule] }
InstImplRuleList  : {- empty -}                           { [] }
                  | InstImplRuleList InstImplRule         { $2 : $1 }

InstImpl          :: { InstImpl }
InstImpl          : Var ArgList '{' InstImplRuleList '}'  {% clearDefined >> return (InstImpl $1 (reverse $2) $4) }

{
splitBitsExpr' :: UnsizedBitsExpr -> ([Bit], UnsizedBitsExpr)
splitBitsExpr' (UnsizedConstBitsExpr bs)     = (bs, UnsizedConstBitsExpr [])
splitBitsExpr' (UnsizedEncBitsExpr v)        = ([], UnsizedEncBitsExpr v)
splitBitsExpr' (UnsizedConcatBitsExpr e1 e2) = case splitBitsExpr' e1 of
  (bs1, UnsizedConstBitsExpr []) -> let (bs2, e2') = splitBitsExpr' e2 in (bs1 ++ bs2, e2')
  (bs1, e1')                     -> (bs1, UnsizedConcatBitsExpr e1' e2)

splitBitsExpr :: UnsizedBitsExpr -> LexerMonad ([Bit], UnsizedBitsExpr)
splitBitsExpr e = do
  let e' = splitBitsExpr' $ e
  when (null . fst $ e') $ throwLocalError 0 "Instruction encodings must have a constant prefix"
  return e'

parseError :: Token -> LexerMonad a
parseError _ = throwLocalError 0 "Parse Error"
}
