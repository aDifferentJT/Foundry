{
module Parser
  ( parse
  ) where

import Prelude hiding (fail)

import Lexer(Token(..))
import qualified Lexer

import Utils(Bit(..), zipBy, zip3By)
import AST

import Data.List(intercalate)

import Control.Monad.Fail
}

%name parse
%tokentype { Token }
%monad { Result }
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
  typeTok       { TypeTok $$ }

%left '+' '-'
%left '*' '/'
%%

Proc             :: { Proc }
Proc             : RawProc                               {% case $1 of RawProc regs insts encTypes regEncs instEncs impls -> do
                                                              regs' <- case regs of
                                                                []  -> Failure "No register block"
                                                                [x] -> Success x
                                                                _   -> Failure "More than one register block"
                                                              regs'' <- case zipBy (\(RegType n _) -> n) (\(RegEnc n _) -> n) regs' regEncs of
                                                                (_, (RegType (Var n) _):_, _) -> Failure $ "Register " ++ n ++ " has no encoding"
                                                                (_, _, (RegEnc (Var n) _):_) -> Failure $ "Encoding given for unknown register " ++ n
                                                                (xs, [], []) -> Success $ [Reg n t e | (RegType n t, RegEnc _ e) <- xs]
                                                              insts' <- case insts of
                                                                         []  -> Failure "No instruction block"
                                                                         [x] -> Success x
                                                                         _   -> Failure "More than one instruction block"
                                                              let instName (Var n) vs = intercalate " " (n : ["<" ++ v ++ ">" | (Var v) <- vs])
                                                              insts'' <- case zip3By (\(InstType n _) -> n) (\(InstImpl n _ _) -> n) (\(InstEnc n _ _) -> n) insts' impls instEncs of
                                                                (_, (InstType (Var n) _, _):_, _, _, _, _, _) -> Failure $ "Instruction " ++ n ++ " has no encoding"
                                                                (_, _, (InstType (Var n) _, _):_, _, _, _, _) -> Failure $ "Instruction " ++ n ++ " has no implementation"
                                                                (_, _, _, (InstImpl n vs _, _):_, _, _, _)    -> Failure $ "Implementation and encoding given for unknown instruction " ++ instName n vs
                                                                (_, _, _, _, (InstType (Var n) _):_, _, _)    -> Failure $ "Instruction " ++ n ++ " has no encoding or implementation"
                                                                (_, _, _, _, _, (InstImpl n vs _):_, _)       -> Failure $ "Implementation given for unknown instruction " ++ instName n vs
                                                                (_, _, _, _, _, _, (InstEnc n vs _):_)        -> Failure $ "Encoding given for unknown instruction " ++ instName n vs
                                                                (xs, [], [], [], [], [], [])                     -> Success $ [Inst n ts (vs1, rs) (vs2, e) | (InstType n ts, InstImpl _ vs1 rs, InstEnc _ vs2 e) <- xs]
                                                              return $ Proc regs'' insts'' encTypes
                                                          }

RawProc          :: { RawProc }
RawProc          : {- empty -}                           { RawProc [] [] [] [] [] [] }
                 | RegTypes RawProc                      { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc ($1:regs) insts encTypes regEncs instEncs impls }
                 | InstTypes RawProc                     { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs ($1:insts) encTypes regEncs instEncs impls }
                 | EncType RawProc                       { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts ($1:encTypes) regEncs instEncs impls }
                 | RegEnc RawProc                        { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts encTypes ($1:regEncs) instEncs impls }
                 | InstEnc RawProc                       { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts encTypes regEncs ($1:instEncs) impls }
                 | InstImpl RawProc                      { case $2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts encTypes regEncs instEncs ($1:impls) }

Var              :: { Var }
Var              : varTok                                { Var $1 }

Type             :: { Type }
Type             : typeTok int                           { Dep $1 $2 }
                 | typeTok                               { NonDep $1 }

Bits             :: { [Bit] }
Bits             : bits                                  { $1 }

RegType          :: { RegType }
RegType          : '-' Var ':' Type                      { RegType $2 $4 }

RegTypeList      :: { [RegType] }
RegTypeList      : {- empty -}                           { [] }
                 | RegTypeList RegType                   { $2 : $1 }

RegTypes         :: { [RegType] }
RegTypes         : registers '{' RegTypeList '}'         { $3 }

EncType          :: { EncType }
EncType          : '<' Type '>' ':' Type                 { EncType $2 $5 }

RegEnc           :: { RegEnc }
RegEnc           : '<' Var '>' '=' Bits                  { RegEnc $2 $5 }

BitsExpr         :: { BitsExpr }
BitsExpr         : Bits                                  { ConstBitsExpr $1 }
                 | '<' Var '>'                           { EncBitsExpr $2 }
                 | BitsExpr '++' BitsExpr                { ConcatBitsExpr $1 $3 }

InstEnc          :: { InstEnc }
InstEnc          : '<' Var ArgList '>' '=' BitsExpr      { InstEnc $2 $3 $6 }

TypeList         :: { [Type] }
TypeList         : {- empty -}                           { [] }
                 | TypeList '<' Type '>'                 { $3 : $1 }

InstType         :: { InstType }
InstType         : '-' Var TypeList                      { InstType $2 (reverse $3) }

InstTypeList     :: { [InstType] }
InstTypeList     : {- empty -}                           { [] }
                 | InstTypeList InstType                 { $2 : $1 }

InstTypes        :: { [InstType] }
InstTypes        : instructions '{' InstTypeList '}'     { $3 }

Expr             :: { Expr }
Expr             : '(' Expr ')'                          { $2 }
                 | Var                                   { VarExpr $1 }
                 | int                                   { ConstExpr $1 }
                 | Expr '+' Expr                         { OpExpr Add $1 $3 }
                 | Expr '-' Expr                         { OpExpr Sub $1 $3 }
                 | Expr '*' Expr                         { OpExpr Mul $1 $3 }
                 | Expr '/' Expr                         { OpExpr Div $1 $3 }

ArgList          :: { [Var] }
ArgList          : {- empty -}                           { [] }
                 | ArgList '<' Var '>'                   { $3 : $1 }

InstImplRule     :: { InstImplRule }
InstImplRule     : Var '<-' Expr                         { InstImplRule $1 $3 }

InstImplRuleList :: { [InstImplRule] }
InstImplRuleList : {- empty -}                           { [] }
                 | InstImplRuleList InstImplRule         { $2 : $1 }

InstImpl         :: { InstImpl }
InstImpl         : Var ArgList '{' InstImplRuleList '}'  { InstImpl $1 (reverse $2) $4 }

{
data Result a = Success a | Failure String
  deriving Show

instance Functor Result where
  fmap f (Success x) = Success $ f x
  fmap f (Failure s) = Failure s

instance Applicative Result where
  pure = Success
  (Success f) <*> (Success x) = Success $ f x
  (Failure s) <*> (_        ) = Failure s
  (_        ) <*> (Failure s) = Failure s
  
instance Monad Result where
  (Success x) >>= f = f x
  (Failure s) >>= _ = Failure s

instance MonadFail Result where
  fail = Failure

parseError :: [Token] -> Result a
parseError xs = Failure ("Parse Error on: " ++ show xs)
}
