{
{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RankNTypes #-}
{-|
Module      : Language.Foundry.Parser
Description : The parser
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.Foundry.Parser (parseM, parse', parse, parseFile) where

import ClassyPrelude hiding (readFile)

import Language.Foundry.Parser.AST
import Language.Foundry.Parser.AlexPosn
import Language.Foundry.Parser.Lexer
import Language.Foundry.Parser.Monad
import Language.Foundry.Parser.TypeCheck

import Control.Applicative (liftA2)
import Control.Lens (Lens', over)
import Control.Monad.Trans (lift)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT(ExceptT))
import qualified Control.Monad.Trans.State as State
import Data.List ((!!))
import qualified Data.Map.Strict as Map
import Data.Text.IO (readFile)
}

%name parseM
%tokentype { Locatable Token }
%monad { ParserMonad }
%lexer { readToken >>= } { Locatable EOF _ }
%error { parseError }

%token
  ';'           { Locatable Semi _ }
  leds          { Locatable LedsTok _ }
  led           { Locatable LedTok _ }
  ':'           { Locatable Colon _ }
  '='           { Locatable Equals _ }
  '+'           { Locatable Plus _ }
  '-'           { Locatable Minus _ }
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

RawProcPrefixSemi :: { RawProc }
RawProcPrefixSemi : Maybe(semi) RawProc                                   { $2 }

RawProc           :: { RawProc }
RawProc           : {- empty -}                                           { initialProc }
                  | RegType RawProc                                       { over rawRegs     (uncurry Map.insert . locatableValue $ $1) $2 }
                  | InstType RawProc                                      { over rawInsts    (uncurry Map.insert . locatableValue $ $1) $2 }
                  | ButtonType RawProc                                    { over rawButtons  (uncurry Map.insert . locatableValue $ $1) $2 }
                  | MemoryType RawProc                                    { over rawMemorys  (locatableValue $1 :) $2 }
                  | EncType RawProc                                       { over rawEncTypes (locatableValue $1 :) $2 }
                  | Enc RawProc                                           { over rawEncs     (uncurry Map.insert . locatableValue $ $1) $2 }
                  | Impl RawProc                                          { over rawImpls    (uncurry Map.insert . locatableValue $ $1) $2 }
                  | LedImpls RawProc                                      {%
  fillMaybe (throwLocalError Nothing $1 "More than one LED block") rawLedImpls (return . locatableValue $ $1) $2
}

semi              :: { Locatable Token }
semi              : ';'                                                   { $1 }
                  | semi ';'                                              { $1 <* $2 }
                  | error                                                 { $1 }

close             :: { Locatable Token }
close             : '}'                                                   { $1 }
                  | error                                                 { $1 }

Maybe(p)          : {- empty -}                                           { Nothing }
                  | p                                                     { Just $1 }

Var               :: { Locatable Text }
Var               : varTok                                                { $1 }

Type              :: { Locatable Type }
Type              : regT int                                              { RegT  `fmap` $2 <* $1 }
                  | bitsT int                                             { BitsT `fmap` $2 <* $1 }
                  | intT int                                              { IntT  `fmap` $2 <* $1 }
                  | instT                                                 { InstT <$ $1 }

BitsExpr          :: { Locatable MaybeBitsExpr }
BitsExpr          : '(' BitsExpr ')'                                      { $2 }
                  | bits                                                  { MaybeConstBitsExpr `fmap` $1 }
                  | '<' Var '>'                                           {%
  getLocalVarEncWidth $2 >>= \case
    Just n  -> return $ MaybeEncBitsExpr (Just n) `fmap` $2 <* $1 <* $3
    Nothing -> checkLocalVar $2 >> (return . pure $ MaybeConstBitsExpr [])
}
                  | BitsExpr '++' BitsExpr                                {
  MaybeConcatBitsExpr (liftM2 (+) (sizeOfMaybeEnc . locatableValue $ $1) (sizeOfMaybeEnc . locatableValue $ $3)) `fmap` $1 <*> $3
}

ArgTypeList       :: { Locatable [Type] }
ArgTypeList       : {- empty -}                                           { pure [] }
                  | ArgTypeList '<' Type '>'                              { liftA2 (:) $3 $1 <* $2 <* $4 }

RegType           :: { Locatable RegType }
RegType           : Var ':' regT int semi                                 {% defineReg $1 $4 }

InstType          :: { Locatable InstType }
InstType          : Var ':' instT ArgTypeList semi                        {% defineInst $1 (reverse `fmap` $4) }

ButtonType        :: { Locatable ButtonType }
ButtonType        : Var ':' buttonT int semi                              {% defineButton $1 $4 }

MemoryType        :: { Locatable Memory }
MemoryType        : Var ':' ramT int int semi                             {% defineMemory $1 $4 $5 }

List(p)           : {- empty -}                                           { pure [] }
                  | List(p) p                                             { liftA2 (:) $2 $1 }

EncType           :: { Locatable EncType }
EncType           : '<' Type '>' ':' bitsT int semi                       {% fmap (<* $1) $ defineEncType $2 $6 }

ArgList           :: { Locatable [Locatable Text] }
ArgList           : {- empty -}                                           { pure [] }
ArgList           : ArgList '<' Var '>'                                   { ($3:) `fmap` $1 <* $2 <* $4 }

VarWithArgs       :: { (Locatable Text, Locatable Defn, Locatable [Text]) }
VarWithArgs       : Var ArgList                                           {%
  do
    defn <- getIdentifierDefn $1
    case locatableValue defn of
      Just (RegDefn n)        -> do
        unless (null . locatableValue $ $2) . throwLocalError () $2 $ "Register " ++ locatableValue $1 ++ " has arguments"
        return ($1, RegDefn n <$ defn, pure [])
      Just (InstDefn ts)      -> do
        unless (length ts == (length . locatableValue $ $2)) . throwLocalError () $2 $ "Instruction " ++ locatableValue $1 ++ " has " ++ tshow (length . locatableValue $ $2) ++ " arguments, expected " ++ tshow (length ts)
        vs <- fmap sequenceA . sequence . map (uncurry defineLocalVar) . flip zip ts . reverse . locatableValue $ $2
        return ($1, InstDefn ts <$ defn, vs)
      Just ButtonDefn         -> do
        unless (null . locatableValue $ $2) . throwLocalError () $2 $ "Button " ++ locatableValue $1 ++ " has arguments"
        return ($1, ButtonDefn <$ defn, pure [])
      Just (MemoryDefn w1 w2) -> do
        unless (null . locatableValue $ $2) . throwLocalError () $2 $ "Memory " ++ locatableValue $1 ++ " has arguments"
        return ($1, MemoryDefn w1 w2 <$ defn, pure [])
      Nothing -> do
        --vs <- fmap sequenceA . sequence . map (uncurry defineLocalVar) . flip zip ts . reverse . locatableValue $ $2
        return ($1, InstDefn [] <$ defn, pure [])
}

Enc               :: { Locatable (Text, Enc) }
Enc               : '<' VarWithArgs '>' '=' BitsExpr semi                 {% fmap (<* $1) $
  do
    clearLocalVars
    let (var, defn, args) = $2
    case defn of
      Locatable (RegDefn n)     ps -> do
        case locatableValue $5 of
          MaybeConstBitsExpr bs -> do
            (getEncWidth . Locatable (RegT n) $ ps) >>= \case
              Just d1 ->
                let d2 = length bs in
                unless (d1 == d2)
                . throwLocalError () $5
                $ "<" ++ locatableValue var ++ "> is of type Bits " ++ tshow d2 ++ " but I expected Bits " ++ tshow d1
              Nothing -> return ()
            return $ (,) `fmap` var <*> (RegEnc `fmap` pure bs) <* $1 <* $5
          _                ->
            throwLocalError (pure ("", RegEnc []) <* $1 <* $5) $5
            $ "Encoding for register " ++ locatableValue var ++ " is not constant"
      Locatable (InstDefn ts)   ps -> do
        addEncoded var
        (getEncWidth . Locatable InstT $ ps) >>= \case
          Just d1 ->
            case sizeOfMaybeEnc . locatableValue $ $5 of
              Just d2 ->
                unless (d1 == d2) . throwLocalError () $5
                $ "<" ++ intercalate " " (locatableValue var : ["<" ++ v ++ ">" | v <- locatableValue args])
                ++ "> is of type Bits " ++ tshow d2
                ++ " but I expected Bits " ++ tshow d1
              Nothing -> return ()
          Nothing -> return ()
        e <- fmap (<$ $5) ((recover (ConstBitsExpr []) . unmaybeBitsExpr . locatableValue $ $5) >>= encPrefix)
        when (null . fst . locatableValue $ e) $ throwLocalError () e "Instruction encodings must have a constant prefix"
        return $ (,) `fmap` var <*> (InstEnc `fmap` args <*> e) <* $1
      Locatable  ButtonDefn      _ -> do
        throwLocalError ((,) `fmap` var <*> (RegEnc `fmap` pure []) <* $1 <* $5) var $ "Encoding given for button " ++ locatableValue var
      Locatable (MemoryDefn _ _) _ -> do
        throwLocalError ((,) `fmap` var <*> (RegEnc `fmap` pure []) <* $1 <* $5) var $ "Encoding given for memory " ++ locatableValue var
}

BoolExpr          :: { Locatable BoolExpr }
BoolExpr          : Expr '==' Expr                                        { EqualityExpr    `fmap` $1 <*> $3 }
                  | Expr '!=' Expr                                        { InequalityExpr `fmap` $1 <*> $3 }
                  | BoolExpr '&&' BoolExpr                                { LogicalAndExpr  `fmap` $1 <*> $3 }
                  | BoolExpr '||' BoolExpr                                { LogicalOrExpr   `fmap` $1 <*> $3 }

Expr              :: { Locatable Expr }
Expr              : '(' Expr ')'                                          { $2 <* $1 <* $3 }
                  | Var                                                   {%
  getLocalVarValWidth $1 >>= \case
    Just n  -> return $ VarExpr (Just n) `fmap` $1
    Nothing -> do
      defn <- getIdentifierDefn $1
      case locatableValue defn of
        Just (RegDefn n)      -> return $ RegExpr (Just n) `fmap` $1
        Just (InstDefn _)     ->
          throwLocalError (RegExpr Nothing `fmap` $1) $1
          $ locatableValue $1 ++ " is an instruction, expected a register or a local variable"
        Just ButtonDefn       ->
          throwLocalError (RegExpr Nothing `fmap` $1) $1
          $ locatableValue $1 ++ " is a button, expected a register or a local variable"
        Just (MemoryDefn _ _) ->
          throwLocalError (RegExpr Nothing `fmap` $1) $1
          $ locatableValue $1 ++ " is a memory, expected a register or a local variable"
        Nothing               -> return $ RegExpr Nothing `fmap` $1
}
                  | Var '[' Expr ']'                                      {% getMemoryWidth $1 >>= \n -> return $ MemAccessExpr n `fmap` $1 <*> $3 }
                  | int                                                   { ConstExpr `fmap` $1 }
                  | bits                                                  { BinaryConstExpr `fmap` $1 }
                  | Expr '+' Expr                                         {% makeOpExpr Add $1 $3 }
                  | Expr '-' Expr                                         {% makeOpExpr Sub $1 $3 }
                  | Expr '*' Expr                                         {% makeOpExpr Mul $1 $3 }
                  | Expr '/' Expr                                         {% makeOpExpr Div $1 $3 }
                  | Expr '++' Expr                                        {% makeOpExpr ConcatBits $1 $3 }
                  | Expr '&' Expr                                         {% makeOpExpr BitwiseAnd $1 $3 }
                  | Expr '|' Expr                                         {% makeOpExpr BitwiseOr $1 $3 }
                  | Expr '^' Expr                                         {% makeOpExpr BitwiseXor $1 $3 }
                  | BoolExpr '?' Expr ':' Expr                            {% makeTernaryExpr $1 $3 $5 }

ImplRule          :: { Locatable ImplRule }
ImplRule          : Var '<-' Expr semi                                    {%
  getLocalVarValWidth $1 >>= \case
    Just d1 -> do
      case widthOfExpr . locatableValue $ $3 of
        Just d2 ->
          unless (d1 == d2) . throwLocalError () $3
          $ "Expression of width " ++ tshow d2 ++ " assigned to " ++ locatableValue $1 ++ " which is of width " ++ tshow d1
        Nothing -> return ()
      return $ ImplRule `fmap` (VarLValue `fmap` $1) <*> $3
    Nothing -> do
      defn <- getIdentifierDefn $1
      case locatableValue defn of
        Just (RegDefn d1)     -> do
          case widthOfExpr . locatableValue $ $3 of
            Just d2 ->
              unless (d1 == d2) . throwLocalError () $3
              $ "Expression of width " ++ tshow d2 ++ " assigned to " ++ locatableValue $1 ++ " which is of width " ++ tshow d1
            Nothing -> return ()
          return $ ImplRule `fmap` (RegLValue `fmap` $1) <*> $3
        Just (InstDefn _)     ->
          throwLocalError (ImplRule `fmap` (RegLValue `fmap` $1) <*> $3) $1
          $ locatableValue $1 ++ " is an instruction, expected a register or a local variable"
        Just ButtonDefn       ->
          throwLocalError (ImplRule `fmap` (RegLValue `fmap` $1) <*> $3) $1
          $ locatableValue $1 ++ " is a button, expected a register or a local variable"
        Just (MemoryDefn _ _) ->
          throwLocalError (ImplRule `fmap` (RegLValue `fmap` $1) <*> $3) $1
          $ locatableValue $1 ++ " is a memory, expected a register or a local variable"
        Nothing               -> return $ ImplRule `fmap` (RegLValue `fmap` $1) <*> $3
}

                  | Var '[' Expr ']' '<-' Expr semi                       {%
  do
    getMemoryWidth $1 >>= \case
      Just d1 ->
        case widthOfExpr . locatableValue $ $6 of
          Just d2 ->
            unless (d1 == d2) . throwLocalError () $6
            $ "Expression of width " ++ tshow d2 ++ " assigned to " ++ locatableValue $1 ++ " which is of width " ++ tshow d1
          Nothing -> return ()
      Nothing -> return ()
    return (ImplRule `fmap` (MemAccessLValue `fmap` $1 <*> $3) <*> $6)
}

Impl              :: { Locatable (Text, Impl) }
Impl              : VarWithArgs '{' Maybe(semi) List(ImplRule) close semi {% fmap (<* $5) $
  do
    clearLocalVars
    let (var, defn, args) = $1
    addImplemented var
    case locatableValue defn of
      (RegDefn _)      -> throwLocalError ((,) `fmap` var <*> (ButtonImpl `fmap` $4)) var $ "Implementation given for register " ++ locatableValue var
      (InstDefn ts)    -> return $ (,) `fmap` var <*> (InstImpl `fmap` args <*> $4)
      ButtonDefn       -> return $ (,) `fmap` var <*> (ButtonImpl `fmap` $4)
      (MemoryDefn _ _) -> throwLocalError ((,) `fmap` var <*>(ButtonImpl `fmap` $4)) var $ "Implementation given for memory " ++ locatableValue var
}

LedImpl           :: { Locatable LedImpl }
LedImpl           : led '[' int ']' '<-' Expr semi                        {%
  do
    case widthOfExpr . locatableValue $ $6 of
      Just d2 ->
        unless (1 == d2) . throwLocalError () $6
        $ "Expression of width " ++ tshow d2 ++ " assigned to a single LED"
      Nothing -> return ()
    return $ LedImpl `fmap` $3 <*> $3 <*> $6 <* $1
}
                  | led '[' int ':' int ']' '<-' Expr semi                {%
  do
    let d1 = abs (locatableValue $5 - locatableValue $3) + 1
    case widthOfExpr . locatableValue $ $8 of
      Just d2 ->
        unless (d1 == d2) . throwLocalError () $8
        $ "Expression of width " ++ tshow d2 ++ " assigned to a range of " ++ tshow d1 ++ " LEDs"
      Nothing -> return ()
    return $ LedImpl `fmap` $3 <*> $5 <*> $8 <* $1
}

LedImpls          :: { Locatable [LedImpl] }
LedImpls          : leds '{' Maybe(semi) List(LedImpl) close semi         { $4 <* $1 <* $5 }

{
fillMaybe :: Functor f => f (Maybe a) -> Lens' s (Maybe a) -> f a -> s -> f s
fillMaybe err l x = l f
  where f Nothing  = Just <$> x
        f (Just _) = err

parseError :: Locatable Token -> ParserMonad a
parseError = flip throwLocalError' "Parse Error"

-- | Parse the given string and return either an error together with a range or a `Proc'
parse' :: Text -> Either [(Text, Maybe (AlexPosn, AlexPosn))] Proc
parse' = runParser' . unrecover $ parseM >>= \x -> throwUnimpl >> (return . typeCheck $ x)

-- | Parse the given string and return either a nicely formatted error or a `Proc'
parse :: Text -> Either Text Proc
parse = runParser . unrecover $ parseM >>= \x -> throwUnimpl >> (return . typeCheck $ x)

-- | Parse the given file and return either a nicely formatted error or a `Proc'
parseFile :: FilePath -> ExceptT Text IO Proc
parseFile = ExceptT . fmap parse . readFile
}
