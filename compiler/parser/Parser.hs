{-# OPTIONS_GHC -w #-}
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
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Proc)
	| HappyAbsSyn5 (RawProc)
	| HappyAbsSyn6 (Var)
	| HappyAbsSyn7 (Type)
	| HappyAbsSyn8 ([Bit])
	| HappyAbsSyn9 (RegType)
	| HappyAbsSyn10 ([RegType])
	| HappyAbsSyn12 (EncType)
	| HappyAbsSyn13 (RegEnc)
	| HappyAbsSyn14 (BitsExpr)
	| HappyAbsSyn15 (InstEnc)
	| HappyAbsSyn16 ([Type])
	| HappyAbsSyn17 (InstType)
	| HappyAbsSyn18 ([InstType])
	| HappyAbsSyn20 (Expr)
	| HappyAbsSyn21 ([Var])
	| HappyAbsSyn22 (InstImplRule)
	| HappyAbsSyn23 ([InstImplRule])
	| HappyAbsSyn24 (InstImpl)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83 :: () => Int -> ({-HappyReduction (Result) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Result) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Result) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Result) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41 :: () => ({-HappyReduction (Result) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Result) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Result) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Result) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,175) ([0,768,1028,0,32864,128,0,0,0,0,0,0,12288,16448,0,1536,2056,0,192,257,0,8216,32,0,1027,4,24576,32896,0,0,64,0,0,8,0,0,192,0,0,0,0,0,0,0,0,0,0,8,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,80,0,0,512,0,0,0,0,16400,0,0,2050,0,0,0,0,1024,0,0,32768,1,0,64,0,0,32768,0,0,1,0,0,4096,0,0,0,0,0,32,0,0,0,0,0,0,0,4096,0,0,0,0,0,66,0,4096,0,0,0,0,0,16,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,520,0,0,0,0,0,0,0,0,0,0,32,0,0,4096,0,0,2,0,0,128,0,32768,12,0,0,0,16384,7,0,0,1600,0,0,0,0,0,0,0,0,4,0,128,0,0,520,0,0,0,0,0,0,0,2048,0,0,4125,0,0,6400,0,0,800,0,0,100,0,32768,12,0,0,0,0,0,0,49152,0,0,6144,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Proc","RawProc","Var","Type","Bits","RegType","RegTypeList","RegTypes","EncType","RegEnc","BitsExpr","InstEnc","TypeList","InstType","InstTypeList","InstTypes","Expr","ArgList","InstImplRule","InstImplRuleList","InstImpl","registers","instructions","':'","'-'","'='","'+'","'*'","'/'","'++'","'<-'","'<'","'>'","'{'","'}'","'('","')'","bits","int","varTok","typeTok","%eof"]
        bit_start = st * 45
        bit_end = (st + 1) * 45
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..44]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (25) = happyShift action_10
action_0 (26) = happyShift action_11
action_0 (35) = happyShift action_12
action_0 (43) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (11) = happyGoto action_4
action_0 (12) = happyGoto action_5
action_0 (13) = happyGoto action_6
action_0 (15) = happyGoto action_7
action_0 (19) = happyGoto action_8
action_0 (24) = happyGoto action_9
action_0 _ = happyReduce_2

action_1 (25) = happyShift action_10
action_1 (26) = happyShift action_11
action_1 (35) = happyShift action_12
action_1 (43) = happyShift action_13
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (11) = happyGoto action_4
action_1 (12) = happyGoto action_5
action_1 (13) = happyGoto action_6
action_1 (15) = happyGoto action_7
action_1 (19) = happyGoto action_8
action_1 (24) = happyGoto action_9
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (21) = happyGoto action_26
action_3 _ = happyReduce_36

action_4 (25) = happyShift action_10
action_4 (26) = happyShift action_11
action_4 (35) = happyShift action_12
action_4 (43) = happyShift action_13
action_4 (5) = happyGoto action_25
action_4 (6) = happyGoto action_3
action_4 (11) = happyGoto action_4
action_4 (12) = happyGoto action_5
action_4 (13) = happyGoto action_6
action_4 (15) = happyGoto action_7
action_4 (19) = happyGoto action_8
action_4 (24) = happyGoto action_9
action_4 _ = happyReduce_2

action_5 (25) = happyShift action_10
action_5 (26) = happyShift action_11
action_5 (35) = happyShift action_12
action_5 (43) = happyShift action_13
action_5 (5) = happyGoto action_24
action_5 (6) = happyGoto action_3
action_5 (11) = happyGoto action_4
action_5 (12) = happyGoto action_5
action_5 (13) = happyGoto action_6
action_5 (15) = happyGoto action_7
action_5 (19) = happyGoto action_8
action_5 (24) = happyGoto action_9
action_5 _ = happyReduce_2

action_6 (25) = happyShift action_10
action_6 (26) = happyShift action_11
action_6 (35) = happyShift action_12
action_6 (43) = happyShift action_13
action_6 (5) = happyGoto action_23
action_6 (6) = happyGoto action_3
action_6 (11) = happyGoto action_4
action_6 (12) = happyGoto action_5
action_6 (13) = happyGoto action_6
action_6 (15) = happyGoto action_7
action_6 (19) = happyGoto action_8
action_6 (24) = happyGoto action_9
action_6 _ = happyReduce_2

action_7 (25) = happyShift action_10
action_7 (26) = happyShift action_11
action_7 (35) = happyShift action_12
action_7 (43) = happyShift action_13
action_7 (5) = happyGoto action_22
action_7 (6) = happyGoto action_3
action_7 (11) = happyGoto action_4
action_7 (12) = happyGoto action_5
action_7 (13) = happyGoto action_6
action_7 (15) = happyGoto action_7
action_7 (19) = happyGoto action_8
action_7 (24) = happyGoto action_9
action_7 _ = happyReduce_2

action_8 (25) = happyShift action_10
action_8 (26) = happyShift action_11
action_8 (35) = happyShift action_12
action_8 (43) = happyShift action_13
action_8 (5) = happyGoto action_21
action_8 (6) = happyGoto action_3
action_8 (11) = happyGoto action_4
action_8 (12) = happyGoto action_5
action_8 (13) = happyGoto action_6
action_8 (15) = happyGoto action_7
action_8 (19) = happyGoto action_8
action_8 (24) = happyGoto action_9
action_8 _ = happyReduce_2

action_9 (25) = happyShift action_10
action_9 (26) = happyShift action_11
action_9 (35) = happyShift action_12
action_9 (43) = happyShift action_13
action_9 (5) = happyGoto action_20
action_9 (6) = happyGoto action_3
action_9 (11) = happyGoto action_4
action_9 (12) = happyGoto action_5
action_9 (13) = happyGoto action_6
action_9 (15) = happyGoto action_7
action_9 (19) = happyGoto action_8
action_9 (24) = happyGoto action_9
action_9 _ = happyReduce_2

action_10 (37) = happyShift action_19
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (37) = happyShift action_18
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (43) = happyShift action_13
action_12 (44) = happyShift action_17
action_12 (6) = happyGoto action_15
action_12 (7) = happyGoto action_16
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_9

action_14 (45) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (36) = happyShift action_34
action_15 (21) = happyGoto action_33
action_15 _ = happyReduce_36

action_16 (36) = happyShift action_32
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (42) = happyShift action_31
action_17 _ = happyReduce_11

action_18 (18) = happyGoto action_30
action_18 _ = happyReduce_26

action_19 (10) = happyGoto action_29
action_19 _ = happyReduce_14

action_20 _ = happyReduce_8

action_21 _ = happyReduce_4

action_22 _ = happyReduce_7

action_23 _ = happyReduce_6

action_24 _ = happyReduce_5

action_25 _ = happyReduce_3

action_26 (35) = happyShift action_27
action_26 (37) = happyShift action_28
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (43) = happyShift action_13
action_27 (6) = happyGoto action_45
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (23) = happyGoto action_44
action_28 _ = happyReduce_39

action_29 (28) = happyShift action_42
action_29 (38) = happyShift action_43
action_29 (9) = happyGoto action_41
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (28) = happyShift action_39
action_30 (38) = happyShift action_40
action_30 (17) = happyGoto action_38
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_10

action_32 (27) = happyShift action_37
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (35) = happyShift action_27
action_33 (36) = happyShift action_36
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (29) = happyShift action_35
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (41) = happyShift action_55
action_35 (8) = happyGoto action_54
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (29) = happyShift action_53
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (44) = happyShift action_17
action_37 (7) = happyGoto action_52
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_27

action_39 (43) = happyShift action_13
action_39 (6) = happyGoto action_51
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_28

action_41 _ = happyReduce_15

action_42 (43) = happyShift action_13
action_42 (6) = happyGoto action_50
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_16

action_44 (38) = happyShift action_49
action_44 (43) = happyShift action_13
action_44 (6) = happyGoto action_47
action_44 (22) = happyGoto action_48
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (36) = happyShift action_46
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_37

action_47 (34) = happyShift action_61
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_40

action_49 _ = happyReduce_41

action_50 (27) = happyShift action_60
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (16) = happyGoto action_59
action_51 _ = happyReduce_23

action_52 _ = happyReduce_17

action_53 (35) = happyShift action_58
action_53 (41) = happyShift action_55
action_53 (8) = happyGoto action_56
action_53 (14) = happyGoto action_57
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_18

action_55 _ = happyReduce_12

action_56 _ = happyReduce_19

action_57 (33) = happyShift action_69
action_57 _ = happyReduce_22

action_58 (43) = happyShift action_13
action_58 (6) = happyGoto action_68
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (35) = happyShift action_67
action_59 _ = happyReduce_25

action_60 (44) = happyShift action_17
action_60 (7) = happyGoto action_66
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (39) = happyShift action_64
action_61 (42) = happyShift action_65
action_61 (43) = happyShift action_13
action_61 (6) = happyGoto action_62
action_61 (20) = happyGoto action_63
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_30

action_63 (28) = happyShift action_74
action_63 (30) = happyShift action_75
action_63 (31) = happyShift action_76
action_63 (32) = happyShift action_77
action_63 _ = happyReduce_38

action_64 (39) = happyShift action_64
action_64 (42) = happyShift action_65
action_64 (43) = happyShift action_13
action_64 (6) = happyGoto action_62
action_64 (20) = happyGoto action_73
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_31

action_66 _ = happyReduce_13

action_67 (44) = happyShift action_17
action_67 (7) = happyGoto action_72
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (36) = happyShift action_71
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (35) = happyShift action_58
action_69 (41) = happyShift action_55
action_69 (8) = happyGoto action_56
action_69 (14) = happyGoto action_70
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (33) = happyShift action_69
action_70 _ = happyReduce_21

action_71 _ = happyReduce_20

action_72 (36) = happyShift action_83
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (28) = happyShift action_74
action_73 (30) = happyShift action_75
action_73 (31) = happyShift action_76
action_73 (32) = happyShift action_77
action_73 (40) = happyShift action_82
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (39) = happyShift action_64
action_74 (42) = happyShift action_65
action_74 (43) = happyShift action_13
action_74 (6) = happyGoto action_62
action_74 (20) = happyGoto action_81
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (39) = happyShift action_64
action_75 (42) = happyShift action_65
action_75 (43) = happyShift action_13
action_75 (6) = happyGoto action_62
action_75 (20) = happyGoto action_80
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (39) = happyShift action_64
action_76 (42) = happyShift action_65
action_76 (43) = happyShift action_13
action_76 (6) = happyGoto action_62
action_76 (20) = happyGoto action_79
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (39) = happyShift action_64
action_77 (42) = happyShift action_65
action_77 (43) = happyShift action_13
action_77 (6) = happyGoto action_62
action_77 (20) = happyGoto action_78
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_35

action_79 _ = happyReduce_34

action_80 (31) = happyShift action_76
action_80 (32) = happyShift action_77
action_80 _ = happyReduce_32

action_81 (31) = happyShift action_76
action_81 (32) = happyShift action_77
action_81 _ = happyReduce_33

action_82 _ = happyReduce_29

action_83 _ = happyReduce_24

happyReduce_1 = happyMonadReduce 1 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( case happy_var_1 of RawProc regs insts encTypes regEncs instEncs impls -> do
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
                                                              return $ Proc regs'' insts'' encTypes))
	) (\r -> happyReturn (HappyAbsSyn4 r))

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 (RawProc [] [] [] [] [] []
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn5
		 (case happy_var_2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc (happy_var_1:regs) insts encTypes regEncs instEncs impls
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn5
		 (case happy_var_2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs (happy_var_1:insts) encTypes regEncs instEncs impls
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn5
		 (case happy_var_2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts (happy_var_1:encTypes) regEncs instEncs impls
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn5
		 (case happy_var_2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts encTypes (happy_var_1:regEncs) instEncs impls
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn5
		 (case happy_var_2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts encTypes regEncs (happy_var_1:instEncs) impls
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn5
		 (case happy_var_2 of RawProc regs insts encTypes regEncs instEncs impls -> RawProc regs insts encTypes regEncs instEncs (happy_var_1:impls)
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyTerminal (VarTok happy_var_1))
	 =  HappyAbsSyn6
		 (Var happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  7 happyReduction_10
happyReduction_10 (HappyTerminal (Int happy_var_2))
	(HappyTerminal (TypeTok happy_var_1))
	 =  HappyAbsSyn7
		 (Dep happy_var_1 happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyTerminal (TypeTok happy_var_1))
	 =  HappyAbsSyn7
		 (NonDep happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyTerminal (Bits happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happyReduce 4 9 happyReduction_13
happyReduction_13 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (RegType happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_0  10 happyReduction_14
happyReduction_14  =  HappyAbsSyn10
		 ([]
	)

happyReduce_15 = happySpecReduce_2  10 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 11 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 5 12 happyReduction_17
happyReduction_17 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (EncType happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 5 13 happyReduction_18
happyReduction_18 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (RegEnc happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn14
		 (ConstBitsExpr happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (EncBitsExpr happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  14 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (ConcatBitsExpr happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 6 15 happyReduction_22
happyReduction_22 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (InstEnc happy_var_2 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_0  16 happyReduction_23
happyReduction_23  =  HappyAbsSyn16
		 ([]
	)

happyReduce_24 = happyReduce 4 16 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (happy_var_3 : happy_var_1
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (InstType happy_var_2 (reverse happy_var_3)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  18 happyReduction_26
happyReduction_26  =  HappyAbsSyn18
		 ([]
	)

happyReduce_27 = happySpecReduce_2  18 happyReduction_27
happyReduction_27 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_2 : happy_var_1
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 19 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  20 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  20 happyReduction_30
happyReduction_30 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn20
		 (VarExpr happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  20 happyReduction_31
happyReduction_31 (HappyTerminal (Int happy_var_1))
	 =  HappyAbsSyn20
		 (ConstExpr happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  20 happyReduction_32
happyReduction_32 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (OpExpr Add happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  20 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (OpExpr Sub happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  20 happyReduction_34
happyReduction_34 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (OpExpr Mul happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  20 happyReduction_35
happyReduction_35 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (OpExpr Div happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  21 happyReduction_36
happyReduction_36  =  HappyAbsSyn21
		 ([]
	)

happyReduce_37 = happyReduce 4 21 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (happy_var_3 : happy_var_1
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_3  22 happyReduction_38
happyReduction_38 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn22
		 (InstImplRule happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  23 happyReduction_39
happyReduction_39  =  HappyAbsSyn23
		 ([]
	)

happyReduce_40 = happySpecReduce_2  23 happyReduction_40
happyReduction_40 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_2 : happy_var_1
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happyReduce 5 24 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (InstImpl happy_var_1 (reverse happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 45 45 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Registers -> cont 25;
	Instructions -> cont 26;
	Colon -> cont 27;
	Hyphen -> cont 28;
	Equals -> cont 29;
	Plus -> cont 30;
	Times -> cont 31;
	Slash -> cont 32;
	Concat -> cont 33;
	LeftArrow -> cont 34;
	OpenAngle -> cont 35;
	CloseAngle -> cont 36;
	OpenCurly -> cont 37;
	CloseCurly -> cont 38;
	OpenParen -> cont 39;
	CloseParen -> cont 40;
	Bits happy_dollar_dollar -> cont 41;
	Int happy_dollar_dollar -> cont 42;
	VarTok happy_dollar_dollar -> cont 43;
	TypeTok happy_dollar_dollar -> cont 44;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 45 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Result a -> (a -> Result b) -> Result b
happyThen = (>>=)
happyReturn :: () => a -> Result a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Result a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Result a
happyError' = (\(tokens, _) -> parseError tokens)
parse tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4













































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc1b43_0/ghc_2.h" #-}








































































































































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
