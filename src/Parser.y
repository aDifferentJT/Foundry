{
{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Parser (parse, parseFile) where

import Proc
import Parser.AST
import Parser.AlexPosn
import Parser.Lexer
import Parser.Monad

import Utils (Bit(..), zipBy, zip3By)

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
%left '=='
%left '&&' '||'
%%

Proc              :: { Locatable Proc }
Proc              : RawProc                                      {%
  case locatableValue $1 of
    RawProc{..} -> do
      regs' <- case _rawRegs of
        []  -> throwGlobalError "No register block"
        [x] -> return x
        _   -> throwGlobalError "More than one register block"
      let regEncs = filter (\case RegEnc _ _ -> True; _ -> False) _rawEncs
      regs <- case zipBy (\(RegType n _) -> n) (\(RegEnc n _) -> n) regs' regEncs of
        (_, (RegType n _):_, _) -> throwGlobalError $ "Register " ++ n ++ " has no encoding"
        (_, _, (RegEnc n _):_) -> throwGlobalError $ "Encoding given for unknown register " ++ n
        (xs, [], []) -> return $ [Reg n t e | (RegType n t, RegEnc _ e) <- xs]
      insts' <- case _rawInsts of
                 []  -> throwGlobalError "No instruction block"
                 [x] -> return x
                 _   -> throwGlobalError "More than one instruction block"
      let instEncs = filter (\case InstEnc _ _ _ -> True; _ -> False) _rawEncs
      let instImpls = filter (\case InstImpl "always" _ _ -> False; InstImpl _ _ _ -> True; _ -> False) _rawImpls
      let instName n vs = intercalate " " (n : ["<" ++ v ++ ">" | v <- vs])
      insts <- case zip3By (\(InstType n _) -> n) (\(InstImpl n _ _) -> n) (\(InstEnc n _ _) -> n) insts' instImpls instEncs of
        (_, (InstType n _, _):_, _, _, _, _, _)    -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding"
        (_, _, (InstType n _, _):_, _, _, _, _)    -> throwGlobalError $ "Instruction " ++ n ++ " has no implementation"
        (_, _, _, (InstImpl n vs _, _):_, _, _, _) -> throwGlobalError $ "Implementation and encoding given for unknown instruction " ++ instName n vs
        (_, _, _, _, (InstType n _):_, _, _)       -> throwGlobalError $ "Instruction " ++ n ++ " has no encoding or implementation"
        (_, _, _, _, _, (InstImpl n vs _):_, _)    -> throwGlobalError $ "Implementation given for unknown instruction " ++ instName n vs
        (_, _, _, _, _, _, (InstEnc n vs _):_)     -> throwGlobalError $ "Encoding given for unknown instruction " ++ instName n vs
        (xs, [], [], [], [], [], [])               -> return $ [Inst n ts (vs1, rs) (vs2, e) | (InstType n ts, InstImpl _ vs1 rs, InstEnc _ vs2 e) <- xs]
      buttons' <- case _rawButtons of
        []  -> throwGlobalError "No register block"
        [x] -> return x
        _   -> throwGlobalError "More than one register block"
      let buttonImpls = filter (\case ButtonImpl _ _ -> True; _ -> False) _rawImpls
      buttons <- case zipBy (\(ButtonType n _) -> n) (\(ButtonImpl n _) -> n) buttons' buttonImpls of
        (_, (ButtonType n _):_, _) -> throwGlobalError $ "Button " ++ n ++ " has no implementation"
        (_, _, (ButtonImpl n _):_) -> throwGlobalError $ "Implementation given for unknown button " ++ n
        (xs, [], []) -> return $ [Button n t rs | (ButtonType n t, ButtonImpl _ rs) <- xs]
      memorys <- case _rawMemorys of
        []  -> throwGlobalError "No register block"
        [x] -> return x
        _   -> throwGlobalError "More than one register block"
      always <- case filter (\case InstImpl "always" _ _ -> True; _ -> False) _rawImpls of
        []                        -> return []
        [InstImpl "always" [] rs] -> return rs
        _                         -> throwGlobalError "More than one always block"
      let encTypes = _rawEncTypes
      return $ Proc{..} <$ $1
}

RawProc           :: { Locatable RawProc }
RawProc           : {- empty -}                                  { pure $ initialProc }
                  | RegTypes RawProc                             { over rawRegs     `fmap` (fmap (:) $1) <*> $2 }
                  | InstTypes RawProc                            { over rawInsts    `fmap` (fmap (:) $1) <*> $2 }
                  | ButtonTypes RawProc                          { over rawButtons  `fmap` (fmap (:) $1) <*> $2 }
                  | MemoryTypes RawProc                          { over rawMemorys  `fmap` (fmap (:) $1) <*> $2 }
                  | EncType RawProc                              { over rawEncTypes `fmap` (fmap (:) $1) <*> $2 }
                  | Enc RawProc                                  { over rawEncs     `fmap` (fmap (:) $1) <*> $2 }
                  | Impl RawProc                                 { over rawImpls    `fmap` (fmap (:) $1) <*> $2 }

Var               :: { Locatable String }
Var               : varTok                                       { $1 }

Type              :: { Locatable Type }
Type              : regT int                                     { RegT  `fmap` $2 <* $1 }
                  | bitsT int                                    { BitsT `fmap` $2 <* $1 }
                  | intT int                                     { IntT  `fmap` $2 <* $1 }
                  | instT                                        { InstT <$ $1 }

BitsExpr          :: { Locatable UnsizedBitsExpr }
BitsExpr          : '(' BitsExpr ')'                             { $2 }
                  | bits                                         { pure $ UnsizedConstBitsExpr $1 }
                  | '<' Var '>'                                  {% checkLocalVar $2 >> return (UnsizedEncBitsExpr $2 <$ $1 <* $3) }
                  | BitsExpr '++' BitsExpr                       { pure $ UnsizedConcatBitsExpr $1 $3 }
                  | BitsExpr '&' BitsExpr                        { pure $ UnsizedAndBitsExpr    $1 $3 }
                  | BitsExpr '|' BitsExpr                        { pure $ UnsizedOrBitsExpr     $1 $3 }
                  | BitsExpr '^' BitsExpr                        { pure $ UnsizedXorBitsExpr    $1 $3 }

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

Arg               :: { Locatable String }
Arg               : '<' Var '>'                                  {% fmap ((<* $1) . (<* $3)) $ defineLocalVar $2}

Enc               :: { Locatable Enc }
Enc               : '<' Var List(Arg) '>' '=' BitsExpr           {% fmap (<* $1) $
  do
    clearLocalVars
    defn <- getIdentifierDefn $2
    case defn of
      Locatable (RegDefn n)     ps -> do
        unless (null . locatableValue $ $3) . throwLocalError $3 $ "Encoding for register " ++ locatableValue $2 ++ " has arguments"
        case locatableValue $6 of
          UnsizedConstBitsExpr bs -> do
            d1 <- getEncType $ Locatable (RegT n) ps
            let d2 = length . locatableValue $ bs
            unless (d1 == d2) . throwLocalError $6 $ "<" ++ locatableValue $2 ++ "> is of type Bits " ++ show d2 ++ " but I expected Bits " ++ show d1
            return $ RegEnc `fmap` $2 <*> bs
          _                       -> throwLocalError $6 $ "Encoding for register " ++ locatableValue $2 ++ " is not constant"
      Locatable (InstDefn ts)   ps -> do
        let vs = fmap reverse $3
        unless (length ts == (length . locatableValue $ vs)) . throwLocalError vs $ "Instruction " ++ locatableValue $2 ++ " has " ++ show (length . locatableValue $ vs) ++ " arguments, expected " ++ show (length ts)
        d1 <- getEncType $ Locatable InstT ps
        (d2, e) <- instEncDim ts vs $6
        unless (d1 == locatableValue d2) . throwLocalError vs $ "<" ++ intercalate " " (locatableValue $2 : ["<" ++ v ++ ">" | v <- locatableValue vs]) ++ "> is of type Bits " ++ (show . locatableValue $ d2) ++ " but I expected Bits " ++ show d1
        return $ InstEnc `fmap` $2 <*> vs <*> e
      Locatable  ButtonDefn      _ -> throwLocalError $2 $ "Encoding given for button " ++ locatableValue $2
      Locatable (MemoryDefn _ _) _ -> throwLocalError $2 $ "Encoding given for memory " ++ locatableValue $2
}

BoolExpr          :: { Locatable BoolExpr }
BoolExpr          : Expr '==' Expr                               { EqualityExpr   `fmap` $1 <*> $3 }
                  | BoolExpr '&&' BoolExpr                       { LogicalAndExpr `fmap` $1 <*> $3 }
                  | BoolExpr '||' BoolExpr                       { LogicalOrExpr  `fmap` $1 <*> $3 }

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
Impl              : Var List(Arg) '{' List(ImplRule) '}'         {% fmap (<* $5) $
  do
    clearLocalVars
    defn <- getIdentifierDefn $1
    case locatableValue defn of
      (RegDefn _)      -> throwLocalError $1 $ "Implementation given for register " ++ locatableValue $1
      (InstDefn ts)    -> do
        unless (length ts == (length . locatableValue $ $2)) . throwLocalError $2 $ "Instruction " ++ locatableValue $1 ++ " has " ++ show (length . locatableValue $ $2) ++ " arguments, expected " ++ show (length ts)
        return (InstImpl `fmap` $1 <*> (reverse `fmap` $2) <*> $4)
      ButtonDefn       -> do
        unless (null . locatableValue $ $2) . throwLocalError $2 $ "Encoding for button " ++ locatableValue $1 ++ " has arguments"
        return (ButtonImpl `fmap` $1 <*> $4)
      (MemoryDefn _ _) -> throwLocalError $1 $ "Implementation given for memory " ++ locatableValue $1
}

{
parseError :: Locatable Token -> ParserMonad a
parseError = flip throwLocalError "Parse Error"

parse :: String -> Either String Proc
parse = fmap locatableValue . runParser parseM

parseFile :: FilePath -> ExceptT String IO Proc
parseFile = ExceptT . fmap parse . readFile
}
