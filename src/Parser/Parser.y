{
{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Parser.Parser (parse) where

import Proc
import Parser.AST
import Parser.Lexer
import Parser.LexerMonad

import Utils (Bit(..), zipBy, zip3By)

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
  buttons       { Buttons }
  memory        { MemoryTok }
  ':'           { Colon }
  '-'           { Hyphen }
  '='           { Equals }
  '+'           { Plus }
  '*'           { Times }
  '/'           { Slash }
  '&'           { And }
  '|'           { Or }
  '^'           { Xor }
  '++'          { Concat }
  '=='          { Equality }
  '&&'          { LogicalAnd }
  '||'          { LogicalOr }
  '?'           { Question }
  '<-'          { LeftArrow }
  '<'           { OpenAngle }
  '>'           { CloseAngle }
  '{'           { OpenCurly }
  '}'           { CloseCurly }
  '('           { OpenParen }
  ')'           { CloseParen }
  '['           { OpenSquare }
  ']'           { CloseSquare }
  bits          { Bits $$ }
  int           { Int $$ }
  varTok        { VarTok $$ }
  regT          { RegTok }
  bitsT         { BitsTok }
  intT          { IntTok }
  instT         { InstTok }
  buttonT       { ButtonTok }
  ramT          { RAMTok }

%right '?'
%left ':'
%left '++'
%left '+' '-'
%left '*' '/'
%left '&' '|' '^'
%left '=='
%left '&&' '||'
%%

Proc              :: { Proc }
Proc              : RawProc                                      {%
  case $1 of
    RawProc{..} -> do
      regs' <- case rawRegs of
        []  -> throwGlobalError "No register block"
        [x] -> return x
        _   -> throwGlobalError "More than one register block"
      let regEncs = filter (\case RegEnc _ _ -> True; _ -> False) rawEncs
      regs'' <- case zipBy (\(RegType n _) -> n) (\(RegEnc n _) -> n) regs' regEncs of
        (_, (RegType n _):_, _) -> throwGlobalError $ "Register " ++ n ++ " has no encoding"
        (_, _, (RegEnc n _):_) -> throwGlobalError $ "Encoding given for unknown register " ++ n
        (xs, [], []) -> return $ [Reg n t e | (RegType n t, RegEnc _ e) <- xs]
      insts' <- case rawInsts of
                 []  -> throwGlobalError "No instruction block"
                 [x] -> return x
                 _   -> throwGlobalError "More than one instruction block"
      let instEncs = filter (\case InstEnc _ _ _ -> True; _ -> False) rawEncs
      let instImpls = filter (\case InstImpl _ _ _ -> True; _ -> False) rawImpls
      let instName n vs = intercalate " " (n : ["<" ++ v ++ ">" | v <- vs])
      insts'' <- case zip3By (\(InstType n _) -> n) (\(InstImpl n _ _) -> n) (\(InstEnc n _ _) -> n) insts' instImpls instEncs of
        (_, (InstType n _, _):_, _, _, _, _, _)    -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding"
        (_, _, (InstType n _, _):_, _, _, _, _)    -> throwGlobalError $ "Instruction " ++ n ++ " has no implementation"
        (_, _, _, (InstImpl n vs _, _):_, _, _, _) -> throwGlobalError $ "Implementation and encoding given for unknown instruction " ++ instName n vs
        (_, _, _, _, (InstType n _):_, _, _)       -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding or implementation"
        (_, _, _, _, _, (InstImpl n vs _):_, _)    -> throwGlobalError $ "Implementation given for unknown instruction " ++ instName n vs
        (_, _, _, _, _, _, (InstEnc n vs _):_)     -> throwGlobalError $ "Encoding given for unknown instruction " ++ instName n vs
        (xs, [], [], [], [], [], [])               -> return $ [Inst n ts (vs1, rs) (vs2, e) | (InstType n ts, InstImpl _ vs1 rs, InstEnc _ vs2 e) <- xs]
      buttons' <- case rawButtons of
        []  -> throwGlobalError "No register block"
        [x] -> return x
        _   -> throwGlobalError "More than one register block"
      let buttonImpls = filter (\case ButtonImpl _ _ -> True; _ -> False) rawImpls
      buttons'' <- case zipBy (\(ButtonType n _) -> n) (\(ButtonImpl n _) -> n) buttons' buttonImpls of
        (_, (ButtonType n _):_, _) -> throwGlobalError $ "Button " ++ n ++ " has no implementation"
        (_, _, (ButtonImpl n _):_) -> throwGlobalError $ "Implementation given for unknown button " ++ n
        (xs, [], []) -> return $ [Button n t rs | (ButtonType n t, ButtonImpl _ rs) <- xs]
      memory' <- case rawMemory of
        []  -> throwGlobalError "No register block"
        [x] -> return x
        _   -> throwGlobalError "More than one register block"
      return $ Proc regs'' insts'' buttons'' memory' rawEncTypes
}

RawProc           :: { RawProc }
RawProc           : {- empty -}                                  { RawProc [] [] [] [] [] [] [] }
                  | RegTypes RawProc                             { $2 { rawRegs     = $1 : rawRegs     $2 } }
                  | InstTypes RawProc                            { $2 { rawInsts    = $1 : rawInsts    $2 } }
                  | ButtonTypes RawProc                          { $2 { rawButtons  = $1 : rawButtons  $2 } }
                  | MemoryTypes RawProc                          { $2 { rawMemory   = $1 : rawMemory   $2 } }
                  | EncType RawProc                              { $2 { rawEncTypes = $1 : rawEncTypes $2 } }
                  | Enc RawProc                                  { $2 { rawEncs     = $1 : rawEncs     $2 } }
                  | Impl RawProc                                 { $2 { rawImpls    = $1 : rawImpls    $2 } }

Var               :: { String }
Var               : varTok                                       { $1 }

Type              :: { Type }
Type              : regT int                                     { RegT $2 }
                  | bitsT int                                    { BitsT $2 }
                  | intT int                                     { IntT $2 }
                  | instT                                        { InstT }

BitsExpr          :: { UnsizedBitsExpr }
BitsExpr          : '(' BitsExpr ')'                             { $2 }
                  | bits                                         { UnsizedConstBitsExpr $1 }
                  | '<' Var '>'                                  {% checkLocalVar $2 >> return (UnsizedEncBitsExpr $2) }
                  | BitsExpr '++' BitsExpr                       { UnsizedConcatBitsExpr $1 $3 }
                  | BitsExpr '&' BitsExpr                        { UnsizedAndBitsExpr $1 $3 }
                  | BitsExpr '|' BitsExpr                        { UnsizedOrBitsExpr $1 $3 }
                  | BitsExpr '^' BitsExpr                        { UnsizedXorBitsExpr $1 $3 }

ArgTypeList       :: { [Type] }
ArgTypeList       : {- empty -}                                  { [] }
                  | ArgTypeList '<' Type '>'                     { $3 : $1 }

RegType           :: { RegType }
RegType           : '-' Var ':' regT int                         {% defineReg $2 $5 }

InstType          :: { InstType }
InstType          : '-' Var ArgTypeList                          {% defineInst $2 (reverse $3) }

ButtonType        :: { ButtonType }
ButtonType        : '-' Var ':' buttonT int                      {% defineButton $2 $5 }

MemoryType        :: { Memory }
MemoryType        : '-' Var ':' ramT int int                     {% defineMemory $2 $5 $6 }

List(p)           : {- empty -}                                  { [] }
                  | List(p) p                                    { $2 : $1 }

RegTypes          :: { [RegType] }
RegTypes          : registers '{' List(RegType) '}'              { $3 }

InstTypes         :: { [InstType] }
InstTypes         : instructions '{' List(InstType) '}'          { $3 }

ButtonTypes       :: { [ButtonType] }
ButtonTypes       : buttons '{' List(ButtonType) '}'             { $3 }

MemoryTypes       :: { [Memory] }
MemoryTypes       : memory '{' List(MemoryType) '}'              { $3 }

EncType           :: { EncType }
EncType           : '<' Type '>' ':' bitsT int                   {% defineEncType $2 $6 }

Arg               :: { String }
Arg               : '<' Var '>'                                  {% defineLocalVar $2 }

Enc               :: { Enc }
Enc               : '<' Var List(Arg) '>' '=' BitsExpr           {%
  do
    clearLocalVars
    defn <- getIdentifierDefn $2
    case defn of
      (RegDefn n)      -> do
        unless (null $3) . throwLocalError 4 $ "Encoding for register " ++ $2 ++ " has arguments"
        case $6 of
          UnsizedConstBitsExpr bs -> do
            d1 <- getEncType $ RegT n
            let d2 = length bs
            unless (d1 == d2) . throwLocalError 1 $ "<" ++ $2 ++ "> is of type Bits " ++ show d2 ++ " but I expected Bits " ++ show d1
            return $ RegEnc $2 bs
          _                       -> throwLocalError 1 $ "Encoding for register " ++ $2 ++ " is not constant"
      (InstDefn ts)    -> do
        let vs = reverse $3
        unless (length ts == length vs) . throwLocalError 1 $ "Instruction " ++ $2 ++ " has " ++ show (length vs) ++ " arguments, expected " ++ show (length ts) 
        d1 <- getEncType InstT
        (d2, bs, e) <- instEncDim ts vs $6
        unless (d1 == d2) . throwLocalError 1 $ "<" ++ intercalate " " ($2 : ["<" ++ v ++ ">" | v <- vs]) ++ "> is of type Bits " ++ show d2 ++ " but I expected Bits " ++ show d1
        return $ InstEnc $2 vs (bs, e)
      ButtonDefn       -> throwLocalError 1 $ "Encoding given for button " ++ $2
      (MemoryDefn _ _) -> throwLocalError 1 $ "Encoding given for memory " ++ $2
}

BoolExpr          :: { BoolExpr }
BoolExpr          : Expr '==' Expr                               { EqualityExpr $1 $3 }
                  | BoolExpr '&&' BoolExpr                       { LogicalAndExpr $1 $3 }
                  | BoolExpr '||' BoolExpr                       { LogicalOrExpr $1 $3 }

Expr              :: { Expr }
Expr              : '(' Expr ')'                                 { $2 }
                  | Var                                          {%
  do
    loc <- isLocalVar $1
    if loc
    then return $ VarExpr $1
    else do
      defn <- getIdentifierDefn $1
      case defn of
        (RegDefn n)      -> return $ RegExpr $1
        (InstDefn ts)    -> throwLocalError 1 $ $1 ++ " is an instruction, expected a register or a local variable"
        ButtonDefn       -> throwLocalError 1 $ $1 ++ " is a button, expected a register or a local variable"
        (MemoryDefn _ _) -> throwLocalError 1 $ $1 ++ " is a memory, expected a register or a local variable"
}
                  | Var '[' Expr ']'                             {% checkMemoryDefined $1 >> return (MemAccessExpr $1 $3) }
                  | int                                          { ConstExpr $1 }
                  | bits                                         { BinaryConstExpr $1 }
                  | Expr '+' Expr                                { OpExpr Add $1 $3 }
                  | Expr '-' Expr                                { OpExpr Sub $1 $3 }
                  | Expr '*' Expr                                { OpExpr Mul $1 $3 }
                  | Expr '/' Expr                                { OpExpr Div $1 $3 }
                  | Expr '++' Expr                               { OpExpr ConcatBits $1 $3 }
                  | Expr '&' Expr                                { OpExpr BitwiseAnd $1 $3 }
                  | Expr '|' Expr                                { OpExpr BitwiseOr $1 $3 }
                  | Expr '^' Expr                                { OpExpr BitwiseXor $1 $3 }
                  | BoolExpr '?' Expr ':' Expr                   { TernaryExpr $1 $3 $5 }

ImplRule          :: { ImplRule }
ImplRule          : Var '<-' Expr                                {%
  do
    loc <- isLocalVar $1
    if loc
    then return $ ImplRule (VarLValue $1) $3
    else do
      defn <- getIdentifierDefn $1
      case defn of
        (RegDefn n)      -> return $ ImplRule (RegLValue $1) $3
        (InstDefn ts)    -> throwLocalError 1 $ $1 ++ " is an instruction, expected a register or a local variable"
        ButtonDefn       -> throwLocalError 1 $ $1 ++ " is a button, expected a register or a local variable"
        (MemoryDefn _ _) -> throwLocalError 1 $ $1 ++ " is a memory, expected a register or a local variable"
}
                  | Var '[' Expr ']' '<-' Expr                   {% checkMemoryDefined $1 >> return (ImplRule (MemAccessLValue $1 $3) $6) }

Impl              :: { Impl }
Impl              : Var List(Arg) '{' List(ImplRule) '}'         {%
  do
    clearLocalVars
    defn <- getIdentifierDefn $1
    case defn of
      (RegDefn n)      -> throwLocalError 1 $ "Implementation given for register " ++ $1
      (InstDefn ts)    -> do
        unless (length ts == length $2) . throwLocalError 1 $ "Instruction " ++ $1 ++ " has " ++ show (length $2) ++ " arguments, expected " ++ show (length ts) 
        return (InstImpl $1 (reverse $2) $4)
      ButtonDefn       -> do
        unless (null $2) . throwLocalError 1 $ "Encoding for button " ++ $1 ++ " has arguments"
        return (ButtonImpl $1 $4)
      (MemoryDefn _ _) -> throwLocalError 1 $ "Implementation given for memory " ++ $1
}

{
parseError :: Token -> LexerMonad a
parseError _ = throwLocalError 0 "Parse Error"
}
