{
{-# LANGUAGE RecordWildCards, LambdaCase #-}

{-|
Module      : Parser
Description : The parser
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Parser (parse, parseFile) where

import Parser.AST
import Parser.AlexPosn
import Parser.Lexer
import Parser.Monad
import Parser.TypeCheck

import Utils (Bit(..))

import Control.Applicative (liftA2)
import Control.Lens (over)
import Control.Monad.Trans (lift)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT(ExceptT))
import qualified Control.Monad.Trans.State as State
import Data.List(intercalate)
}

%name parseM
%tokentype { Locatable Token }
%monad { ParserMonad }
%lexer { readToken >>= } { Locatable EOF _ }
%error { parseError }

%token
  registers     { Locatable Registers _ }
  instructions  { Locatable Instructions _ }
  buttons       { Locatable Buttons _ }
  memory        { Locatable MemoryTok _ }
  leds          { Locatable LedsTok _ }
  led           { Locatable LedTok _ }
  ':'           { Locatable Colon _ }
  '-'           { Locatable Hyphen _ }
  '='           { Locatable Equals _ }
  '+'           { Locatable Plus _ }
  '*'           { Locatable Times _ }
  '/'           { Locatable Slash _ }
  '&'           { Locatable And _ }
  '|'           { Locatable Or _ }
  '^'           { Locatable Xor _ }
  '++'          { Locatable Concat _ }
  '=='          { Locatable Equality _ }
  '!='          { Locatable Inequality _ }
  '&&'          { Locatable LogicalAnd _ }
  '||'          { Locatable LogicalOr _ }
  '?'           { Locatable Question _ }
  '<-'          { Locatable LeftArrow _ }
  '<'           { Locatable OpenAngle _ }
  '>'           { Locatable CloseAngle _ }
  '{'           { Locatable OpenCurly _ }
  '}'           { Locatable CloseCurly _ }
  '('           { Locatable OpenParen _ }
  ')'           { Locatable CloseParen _ }
  '['           { Locatable OpenSquare _ }
  ']'           { Locatable CloseSquare _ }
  bits          { Locatable (Bits $$) _ }
  int           { Locatable (Int $$) _ }
  varTok        { Locatable (VarTok $$) _ }
  regT          { Locatable RegTok _ }
  bitsT         { Locatable BitsTok _ }
  intT          { Locatable IntTok _ }
  instT         { Locatable InstTok _ }
  buttonT       { Locatable ButtonTok _ }
  ramT          { Locatable RAMTok _ }

%right '?'
%left ':'
%left '++'
%left '+' '-'
%left '*' '/'
%left '&' '|' '^'
%left '==' '!='
%left '&&' '||'
%%

RawProc           :: { Locatable RawProc }
RawProc           : {- empty -}                                  { pure $ initialProc }
                  | RegTypes RawProc                             { over rawRegs     `fmap` (fmap (:) $1) <*> $2 }
                  | InstTypes RawProc                            { over rawInsts    `fmap` (fmap (:) $1) <*> $2 }
                  | ButtonTypes RawProc                          { over rawButtons  `fmap` (fmap (:) $1) <*> $2 }
                  | MemoryTypes RawProc                          { over rawMemorys  `fmap` (fmap (:) $1) <*> $2 }
                  | EncType RawProc                              { over rawEncTypes `fmap` (fmap (:) $1) <*> $2 }
                  | Enc RawProc                                  { over rawEncs     `fmap` (fmap (:) $1) <*> $2 }
                  | Impl RawProc                                 { over rawImpls    `fmap` (fmap (:) $1) <*> $2 }
                  | LedImpls RawProc                             { over rawLedImpls `fmap` (fmap (:) $1) <*> $2 }

Var               :: { Locatable String }
Var               : varTok                                       { $1 }

Type              :: { Locatable Type }
Type              : regT int                                     { RegT  `fmap` $2 <* $1 }
                  | bitsT int                                    { BitsT `fmap` $2 <* $1 }
                  | intT int                                     { IntT  `fmap` $2 <* $1 }
                  | instT                                        { InstT <$ $1 }

BitsExpr          :: { Locatable BitsExpr }
BitsExpr          : '(' BitsExpr ')'                             { $2 }
                  | bits                                         { ConstBitsExpr `fmap` $1 }
                  | '<' Var '>'                                  {% getLocalVarWidth $2 >>= \w -> return (EncBitsExpr w `fmap` $2 <* $1 <* $3) }
                  | BitsExpr '++' BitsExpr                       {
  ConcatBitsExpr ((sizeOfEnc . locatableValue $ $1) + (sizeOfEnc . locatableValue $ $3)) `fmap` $1 <*> $3
}
                  | BitsExpr '&' BitsExpr                        {%
  do
    let n1 = sizeOfEnc . locatableValue $ $1
    let n2 = sizeOfEnc . locatableValue $ $3
    unless (n1 == n2) $ throwLocalError (liftA2 (,) $1 $3) $ "Mismatched dimensions of bitwise and: Bits " ++ show n1 ++ " and Bits " ++ show n2
    return $ AndBitsExpr (n1 + n2) `fmap` $1 <*> $3
}
                  | BitsExpr '|' BitsExpr                        {%
  do
    let n1 = sizeOfEnc . locatableValue $ $1
    let n2 = sizeOfEnc . locatableValue $ $3
    unless (n1 == n2) $ throwLocalError (liftA2 (,) $1 $3) $ "Mismatched dimensions of bitwise or: Bits " ++ show n1 ++ " and Bits " ++ show n2
    return $ OrBitsExpr (n1 + n2) `fmap` $1 <*> $3
}
                  | BitsExpr '^' BitsExpr                        {%
  do
    let n1 = sizeOfEnc . locatableValue $ $1
    let n2 = sizeOfEnc . locatableValue $ $3
    unless (n1 == n2) $ throwLocalError (liftA2 (,) $1 $3) $ "Mismatched dimensions of bitwise exclusive or: Bits " ++ show n1 ++ " and Bits " ++ show n2
    return $ XorBitsExpr (n1 + n2) `fmap` $1 <*> $3
}

ArgTypeList       :: { Locatable [Type] }
ArgTypeList       : {- empty -}                                  { pure [] }
                  | ArgTypeList '<' Type '>'                     { liftA2 (:) $3 $1 <* $2 <* $4 }

RegType           :: { Locatable RegType }
RegType           : '-' Var ':' regT int                         {% fmap (<* $1) $ defineReg $2 $5 }

InstType          :: { Locatable InstType }
InstType          : '-' Var ArgTypeList                          {% fmap (<* $1) $ defineInst $2 (reverse `fmap` $3) }

ButtonType        :: { Locatable ButtonType }
ButtonType        : '-' Var ':' buttonT int                      {% fmap (<* $1) $ defineButton $2 $5 }

MemoryType        :: { Locatable Memory }
MemoryType        : '-' Var ':' ramT int int                     {% fmap (<* $1) $ defineMemory $2 $5 $6 }

List(p)           : {- empty -}                                  { pure [] }
                  | List(p) p                                    { liftA2 (:) $2 $1 }

RegTypes          :: { Locatable [RegType] }
RegTypes          : registers '{' List(RegType) '}'              { $3 }

InstTypes         :: { Locatable [InstType] }
InstTypes         : instructions '{' List(InstType) '}'          { $3 }

ButtonTypes       :: { Locatable [ButtonType] }
ButtonTypes       : buttons '{' List(ButtonType) '}'             { $3 }

MemoryTypes       :: { Locatable [Memory] }
MemoryTypes       : memory '{' List(MemoryType) '}'              { $3 }

EncType           :: { Locatable EncType }
EncType           : '<' Type '>' ':' bitsT int                   {% fmap (<* $1) $ defineEncType $2 $6 }

ArgList           :: { Locatable [Locatable String] }
ArgList           : {- empty -}                                  { pure [] }
ArgList           : ArgList '<' Var '>'                          { ($3:) `fmap` $1 <* $2 <* $4 }

VarWithArgs       :: { (Locatable String, Locatable Defn, Locatable [String]) }
VarWithArgs       : Var ArgList                                  {%
  do
    defn <- getIdentifierDefn $1
    case locatableValue defn of
      RegDefn n      -> do
        unless (null . locatableValue $ $2) . throwLocalError $2 $ "Register " ++ locatableValue $1 ++ " has arguments"
        return ($1, defn, pure [])
      InstDefn ts    -> do
        unless (length ts == (length . locatableValue $ $2)) . throwLocalError $2 $ "Instruction " ++ locatableValue $1 ++ " has " ++ show (length . locatableValue $ $2) ++ " arguments, expected " ++ show (length ts)
        vs <- fmap sequenceA . sequence . map (uncurry defineLocalVar) . flip zip ts . reverse . locatableValue $ $2
        return ($1, defn, vs)
      ButtonDefn     -> do
        unless (null . locatableValue $ $2) . throwLocalError $2 $ "Button " ++ locatableValue $1 ++ " has arguments"
        return ($1, defn, pure [])
      MemoryDefn _ _ -> do
        unless (null . locatableValue $ $2) . throwLocalError $2 $ "Memory " ++ locatableValue $1 ++ " has arguments"
        return ($1, defn, pure [])
}

Enc               :: { Locatable Enc }
Enc               : '<' VarWithArgs '>' '=' BitsExpr             {% fmap (<* $1) $
  do
    clearLocalVars
    let (var, defn, args) = $2
    case defn of
      Locatable (RegDefn n)     ps -> do
        case locatableValue $5 of
          ConstBitsExpr bs -> do
            d1 <- getEncType $ Locatable (RegT n) ps
            let d2 = length bs
            unless (d1 == d2) . throwLocalError $5 $ "<" ++ locatableValue var ++ "> is of type Bits " ++ show d2 ++ " but I expected Bits " ++ show d1
            return $ RegEnc `fmap` var <*> pure bs <* $1 <* $5
          _                -> throwLocalError $5 $ "Encoding for register " ++ locatableValue var ++ " is not constant"
      Locatable (InstDefn ts)   ps -> do
        d1 <- getEncType $ Locatable InstT ps
        let d2 = sizeOfEnc . locatableValue $ $5
        e <- encPrefix $5
        unless (d1 == d2) . throwLocalError $5 $ "<" ++ intercalate " " (locatableValue var : ["<" ++ v ++ ">" | v <- locatableValue args]) ++ "> is of type Bits " ++ show d2 ++ " but I expected Bits " ++ show d1
        return $ InstEnc `fmap` var <*> args <*> e <* $1
      Locatable  ButtonDefn      _ -> throwLocalError var $ "Encoding given for button " ++ locatableValue var
      Locatable (MemoryDefn _ _) _ -> throwLocalError var $ "Encoding given for memory " ++ locatableValue var
}

BoolExpr          :: { Locatable BoolExpr }
BoolExpr          : Expr '==' Expr                               { EqualityExpr    `fmap` $1 <*> $3 }
                  | Expr '!=' Expr                               { InequalityExpr `fmap` $1 <*> $3 }
                  | BoolExpr '&&' BoolExpr                       { LogicalAndExpr  `fmap` $1 <*> $3 }
                  | BoolExpr '||' BoolExpr                       { LogicalOrExpr   `fmap` $1 <*> $3 }

Expr              :: { Locatable Expr }
Expr              : '(' Expr ')'                                 { $2 <* $1 <* $3 }
                  | Var                                          {%
  do
    loc <- isLocalVar . locatableValue $ $1
    if loc
    then return $ VarExpr `fmap` $1
    else do
      defn <- getIdentifierDefn $1
      case locatableValue defn of
        (RegDefn _)      -> return $ RegExpr `fmap` $1
        (InstDefn _)     -> throwLocalError $1 $ locatableValue $1 ++ " is an instruction, expected a register or a local variable"
        ButtonDefn       -> throwLocalError $1 $ locatableValue $1 ++ " is a button, expected a register or a local variable"
        (MemoryDefn _ _) -> throwLocalError $1 $ locatableValue $1 ++ " is a memory, expected a register or a local variable"
}
                  | Var '[' Expr ']'                             {% checkMemoryDefined $1 >> return (MemAccessExpr `fmap` $1 <*> $3) }
                  | int                                          { ConstExpr `fmap` $1 }
                  | bits                                         { BinaryConstExpr `fmap` $1 }
                  | Expr '+' Expr                                { OpExpr Add `fmap` $1 <*> $3 }
                  | Expr '-' Expr                                { OpExpr Sub `fmap` $1 <*> $3 }
                  | Expr '*' Expr                                { OpExpr Mul `fmap` $1 <*> $3 }
                  | Expr '/' Expr                                { OpExpr Div `fmap` $1 <*> $3 }
                  | Expr '++' Expr                               { OpExpr ConcatBits `fmap` $1 <*> $3 }
                  | Expr '&' Expr                                { OpExpr BitwiseAnd `fmap` $1 <*> $3 }
                  | Expr '|' Expr                                { OpExpr BitwiseOr `fmap` $1 <*> $3 }
                  | Expr '^' Expr                                { OpExpr BitwiseXor `fmap` $1 <*> $3 }
                  | BoolExpr '?' Expr ':' Expr                   { TernaryExpr `fmap` $1 <*> $3 <*> $5 }

ImplRule          :: { Locatable ImplRule }
ImplRule          : Var '<-' Expr                                {%
  do
    loc <- isLocalVar . locatableValue $ $1
    if loc
    then return $ ImplRule `fmap` (VarLValue `fmap` $1) <*> $3
    else do
      defn <- getIdentifierDefn $1
      case locatableValue defn of
        (RegDefn n)      -> return $ ImplRule `fmap` (RegLValue `fmap` $1) <*> $3
        (InstDefn ts)    -> throwLocalError $1 $ locatableValue $1 ++ " is an instruction, expected a register or a local variable"
        ButtonDefn       -> throwLocalError $1 $ locatableValue $1 ++ " is a button, expected a register or a local variable"
        (MemoryDefn _ _) -> throwLocalError $1 $ locatableValue $1 ++ " is a memory, expected a register or a local variable"
}
                  | Var '[' Expr ']' '<-' Expr                   {% checkMemoryDefined $1 >> return (ImplRule `fmap` (MemAccessLValue `fmap` $1 <*> $3) <*> $6) }

Impl              :: { Locatable Impl }
Impl              : VarWithArgs '{' List(ImplRule) '}'           {% fmap (<* $4) $
  do
    clearLocalVars
    let (var, defn, args) = $1
    case locatableValue defn of
      (RegDefn _)      -> throwLocalError var $ "Implementation given for register " ++ locatableValue var
      (InstDefn ts)    -> do
        return (InstImpl `fmap` var <*> args <*> $3)
      ButtonDefn       -> do
        return (ButtonImpl `fmap` var <*> $3)
      (MemoryDefn _ _) -> throwLocalError var $ "Implementation given for memory " ++ locatableValue var
}

LedImpl           :: { Locatable LedImpl }
LedImpl           : led '[' int ']' '<-' Expr                    { LedImpl `fmap` $3 <*> $3 <*> $6 <* $1 }
                  | led '[' int ':' int ']' '<-' Expr            { LedImpl `fmap` $3 <*> $5 <*> $8 <* $1 }

LedImpls          :: { Locatable [LedImpl] }
LedImpls          : leds '{' List(LedImpl) '}'                   { $3 <* $1 <* $4 }

{
parseError :: Locatable Token -> ParserMonad a
parseError = flip throwLocalError "Parse Error"

-- | Parse the given string and return either a nicely formatted error or a `Proc'
parse :: String -> Either String Proc
parse = runParser (fmap locatableValue parseM >>= typeCheck)

-- | Parse the given file and return either a nicely formatted error or a `Proc'
parseFile :: FilePath -> ExceptT String IO Proc
parseFile = ExceptT . fmap parse . readFile
}
