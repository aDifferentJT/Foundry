{-|
Module      : Proc
Description : The data structure representing a processor
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental

This is the data structure representing a processor to be passed between modules
-}
module Proc
  ( Type(..)
  , EncType(..)
  , BitsExpr(..)
  , sizeOfEnc
  , findVarInEnc
  , Op(..)
  , BoolExpr(..)
  , Expr(..)
  , LValue(..)
  , ImplRule(..)
  , LedImpl(..)
  , Reg(..)
  , Inst(..)
  , Button(..)
  , Memory(..)
  , Proc(..)
  ) where

import Utils (Bit)

-- | The types supported by the type system
data Type
  = RegT Int  -- ^ A register of a given width
  | BitsT Int -- ^ A given number of bits
  | IntT Int  -- ^ An integer of a given number of bits
  | InstT     -- ^ An instruction
  deriving (Eq, Ord, Show)

-- | The width of the encoding of a given type
data EncType = EncType Type Int
  deriving Show

-- | An expression for some bits
data BitsExpr
  = ConstBitsExpr [Bit]                  -- ^ A constant array of bits
  | EncBitsExpr Int String               -- ^ The encoding of a variable with given width and identifier
  | ConcatBitsExpr Int BitsExpr BitsExpr -- ^ Two expressions of bits concatenated with the width
  | AndBitsExpr Int BitsExpr BitsExpr    -- ^ Two expressions of bits combined with a bitwise and operation with the width
  | OrBitsExpr Int BitsExpr BitsExpr     -- ^ Two expressions of bits combined with a bitwise or operation with the width
  | XorBitsExpr Int BitsExpr BitsExpr    -- ^ Two expressions of bits combined with a bitwise exclusive or operation with the width
  deriving Show

-- | Calculate the width of an expression of bits
sizeOfEnc :: BitsExpr -> Int
sizeOfEnc (ConstBitsExpr  bs)    = length bs
sizeOfEnc (EncBitsExpr    n _)   = n
sizeOfEnc (ConcatBitsExpr n _ _) = n
sizeOfEnc (AndBitsExpr    n _ _) = n
sizeOfEnc (OrBitsExpr     n _ _) = n
sizeOfEnc (XorBitsExpr    n _ _) = n

-- | Return the range of bits representing a given variable in the expression 
findVarInEnc :: String           -- ^ The identifier of the variable
             -> Int              -- ^ The starting index in the expression
             -> BitsExpr         -- ^ The expression
             -> Maybe (Int, Int) -- ^ The range of bits (inclusive) representing @var@
findVarInEnc _   _   (ConstBitsExpr _) = Nothing
findVarInEnc var off (EncBitsExpr n var')
  | var == var' = Just (off, off + n - 1)
  | otherwise   = Nothing
findVarInEnc var off (ConcatBitsExpr _ e1 e2) =
  case findVarInEnc var off e1 of
    Just res -> Just res
    Nothing  -> findVarInEnc var (off + sizeOfEnc e1) e2
findVarInEnc _   _   AndBitsExpr{} = Nothing
findVarInEnc _   _   OrBitsExpr{}  = Nothing
findVarInEnc _   _   XorBitsExpr{} = Nothing

-- | A binary operation
data Op          
  = Add        -- ^ Addition
  | Sub        -- ^ Subtraction
  | Mul        -- ^ Multiplication
  | Div        -- ^ Division
  | ConcatBits -- ^ Concatenate the bits
  | BitwiseAnd -- ^ Bitwise and
  | BitwiseOr  -- ^ Bitwise or
  | BitwiseXor -- ^ Bitwise exclusive or
  deriving Show

-- | An expression for a boolean
data BoolExpr
  = EqualityExpr Expr Expr           -- ^ Equality check
  | LogicalAndExpr BoolExpr BoolExpr -- ^ Logical and
  | LogicalOrExpr BoolExpr BoolExpr  -- ^ Logical or
  deriving Show

-- | An expression
data Expr
  = VarExpr String                 -- ^ An expression representing a variable
  | RegExpr String                 -- ^ An expression representing the value in a register
  | MemAccessExpr String Expr      -- ^ An access into memory
  | ConstExpr Int                  -- ^ A constant
  | BinaryConstExpr [Bit]          -- ^ A binary constant (some bits)
  | OpExpr Op Expr Expr            -- ^ A binary operation
  | TernaryExpr BoolExpr Expr Expr -- ^ A C-style ternary expression
  deriving Show

-- | An expression into which we can assign a value
data LValue
  = VarLValue String            -- ^ An expression representing a variable, this should be a register variable
  | RegLValue String            -- ^ An expression representing a register
  | MemAccessLValue String Expr -- ^ An expression representing a memory location
  deriving Show

-- | A rule for assigning a new value to an lvalue
data ImplRule = ImplRule LValue Expr
  deriving Show

-- | A rule for assigning a value to an LED
data LedImpl = LedImpl Int Int Expr
  deriving Show

-- | A register definition, with identifier, width and possibly an encoding
data Reg = Reg String Int (Maybe [Bit])
  deriving Show

-- | An instruction definition, with identifier, argument types, implementation rules and an encoding
data Inst = Inst String [Type] ([String], [ImplRule]) ([String], ([Bit], BitsExpr))
  deriving Show

-- | A button definition, with identifier, physical index and implementation rules
data Button = Button String Int [ImplRule]
  deriving Show

-- | A memory definition, with identifier, data width and address width
data Memory = Memory String Int Int
  deriving Show

-- | A processor definition
data Proc = Proc
  { regs     :: [Reg]      -- ^ Register definitions
  , insts    :: [Inst]     -- ^ Instruction definitions
  , buttons  :: [Button]   -- ^ Button definitions
  , memorys  :: [Memory]   -- ^ Memory definitions
  , always   :: [ImplRule] -- ^ Various implementation rules that occur on every clock tick
  , leds     :: [LedImpl]  -- ^ Various rules defining the outputs of the LEDs
  , encTypes :: [EncType]  -- ^ The widths of the encodings of various types
  }
  deriving Show
