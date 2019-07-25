{
{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Parser.Parser (parse) where

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

Proc              :: { UnsizedProc }
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
        (xs, [], [], [], [], [], [])               -> return $ [UnsizedInst n ts (vs1, rs) (vs2, e) | (InstType n ts, InstImpl _ vs1 rs, InstEnc _ vs2 e) <- xs]
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
      return $ UnsizedProc regs'' insts'' buttons'' memory' rawEncTypes
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
RegType           : '-' Var ':' regT int                         {% defineReg $2 >> return (RegType $2 $5) }

InstType          :: { InstType }
InstType          : '-' Var ArgTypeList                          {% defineInst $2 >> return (InstType $2 (reverse $3)) }

ButtonType        :: { ButtonType }
ButtonType        : '-' Var ':' buttonT int                      {% defineButton $2 >> return (ButtonType $2 $5) }

MemoryType        :: { Memory }
MemoryType        : '-' Var ':' ramT int int                     {% defineMemory $2 >> return (Memory $2 $5 $6) }

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
EncType           : '<' Type '>' ':' bitsT int                   { EncType $2 $6 }

Arg               :: { String }
Arg               : '<' Var '>'                                  {% defineLocalVar $2 >> return $2 }

Enc               :: { Enc }
Enc               : '<' Var List(Arg) '>' '=' BitsExpr           {%
  do
    clearLocalVars
    defn <- getIdentifierDefn $2
    case defn of
      RegDefn    -> do
        unless (null $3) . throwLocalError 4 $ "Encoding for register " ++ $2 ++ " has arguments"
        case $6 of
          UnsizedConstBitsExpr bs -> return $ RegEnc $2 bs
          _                       -> throwLocalError 1 $ "Encoding for register " ++ $2 ++ " is not constant"
      InstDefn   -> fmap (InstEnc $2 $3) (splitBitsExpr $6)
      ButtonDefn -> throwLocalError 1 $ "Encoding given for button " ++ $2
      MemoryDefn -> throwLocalError 1 $ "Encoding given for memory " ++ $2
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
        RegDefn    -> return $ RegExpr $1
        InstDefn   -> throwLocalError 1 $ $1 ++ " is an instruction, expected a register or a local variable"
        ButtonDefn -> throwLocalError 1 $ $1 ++ " is a button, expected a register or a local variable"
        MemoryDefn -> throwLocalError 1 $ $1 ++ " is a memory, expected a register or a local variable"
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
        RegDefn    -> return $ ImplRule (RegLValue $1) $3
        InstDefn   -> throwLocalError 1 $ $1 ++ " is an instruction, expected a register or a local variable"
        ButtonDefn -> throwLocalError 1 $ $1 ++ " is a button, expected a register or a local variable"
        MemoryDefn -> throwLocalError 1 $ $1 ++ " is a memory, expected a register or a local variable"
}
                  | Var '[' Expr ']' '<-' Expr                   {% checkMemoryDefined $1 >> return (ImplRule (MemAccessLValue $1 $3) $6) }

Impl              :: { Impl }
Impl              : Var List(Arg) '{' List(ImplRule) '}'         {%
  do
    clearLocalVars
    defn <- getIdentifierDefn $1
    case defn of
      RegDefn    -> throwLocalError 1 $ "Implementation given for register " ++ $1
      InstDefn   -> return (InstImpl $1 (reverse $2) $4)
      ButtonDefn -> do
        unless (null $2) . throwLocalError 4 $ "Encoding for button " ++ $1 ++ " has arguments"
        return (ButtonImpl $1 $4)
      MemoryDefn -> throwLocalError 1 $ "Implementation given for memory " ++ $1
}

{
splitBitsExpr' :: UnsizedBitsExpr -> ([Bit], UnsizedBitsExpr)
splitBitsExpr' (UnsizedConstBitsExpr bs)     = (bs, UnsizedConstBitsExpr [])
splitBitsExpr' (UnsizedEncBitsExpr v)        = ([], UnsizedEncBitsExpr v)
splitBitsExpr' (UnsizedConcatBitsExpr e1 e2) = case splitBitsExpr' e1 of
  (bs1, UnsizedConstBitsExpr []) -> let (bs2, e2') = splitBitsExpr' e2 in (bs1 ++ bs2, e2')
  (bs1, e1')                     -> (bs1, UnsizedConcatBitsExpr e1' e2)
splitBitsExpr' (UnsizedAndBitsExpr e1 e2)    = ([], UnsizedAndBitsExpr e1 e2)
splitBitsExpr' (UnsizedOrBitsExpr e1 e2)     = ([], UnsizedOrBitsExpr e1 e2)
splitBitsExpr' (UnsizedXorBitsExpr e1 e2)    = ([], UnsizedXorBitsExpr e1 e2)

splitBitsExpr :: UnsizedBitsExpr -> LexerMonad ([Bit], UnsizedBitsExpr)
splitBitsExpr e = do
  let e' = splitBitsExpr' $ e
  when (null . fst $ e') $ throwLocalError 0 "Instruction encodings must have a constant prefix"
  return e'

parseError :: Token -> LexerMonad a
parseError _ = throwLocalError 0 "Parse Error"
}
