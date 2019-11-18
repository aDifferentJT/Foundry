{-# LANGUAGE ExistentialQuantification, FlexibleInstances, NoImplicitPrelude, OverloadedStrings, RankNTypes, TupleSections #-}

{-|
Module      : Language.Elm.AST
Description : A simple Elm AST
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.Elm.AST
  ( ElmType(..)
  , ElmPattern(..)
  , ElmExpr(..)
  , ElmStmt(..)
  , pretty
  ) where

import ClassyPrelude

import Maps.List (mapHead, mapHeadTail, mapLast)

import qualified Data.Text as Text

-- | Check if a list is plural (more than 1 element)
isPlural :: [a] -> Bool
isPlural []      = False
isPlural [_]     = False
isPlural (_:_:_) = True

data ElmAssoc = ElmLeftAssoc | ElmRightAssoc | ElmNoAssoc

instance Eq ElmAssoc where
  ElmLeftAssoc  == ElmLeftAssoc  = True
  ElmRightAssoc == ElmRightAssoc = True
  _             == _             = False

data ElmPrec
  = ElmPrecNone
  | ElmPrec0
  -- | ElmPrec1 -- isn't used
  | ElmPrec2
  | ElmPrec3
  | ElmPrec4
  | ElmPrec5
  | ElmPrec6
  | ElmPrec7
  | ElmPrec8
  | ElmPrec9
  | ElmPrecFuncAppl
  | ElmPrecMember
  | ElmPrecLit
  | ElmPrecParen
  deriving (Eq, Ord)

precOfOp :: Text -> Maybe (ElmAssoc, ElmPrec)
precOfOp ">>" = Just (ElmLeftAssoc,  ElmPrec9)
precOfOp "<<" = Just (ElmRightAssoc, ElmPrec9)
precOfOp "^"  = Just (ElmRightAssoc, ElmPrec8)
precOfOp "*"  = Just (ElmLeftAssoc,  ElmPrec7)
precOfOp "/"  = Just (ElmLeftAssoc,  ElmPrec7)
precOfOp "//" = Just (ElmLeftAssoc,  ElmPrec7)
precOfOp "+"  = Just (ElmLeftAssoc,  ElmPrec6)
precOfOp "-"  = Just (ElmLeftAssoc,  ElmPrec6)
precOfOp "++" = Just (ElmRightAssoc, ElmPrec5)
precOfOp "::" = Just (ElmRightAssoc, ElmPrec5)
precOfOp "==" = Just (ElmNoAssoc,    ElmPrec4)
precOfOp "/=" = Just (ElmNoAssoc,    ElmPrec4)
precOfOp "<"  = Just (ElmNoAssoc,    ElmPrec4)
precOfOp ">"  = Just (ElmNoAssoc,    ElmPrec4)
precOfOp "<=" = Just (ElmNoAssoc,    ElmPrec4)
precOfOp ">=" = Just (ElmNoAssoc,    ElmPrec4)
precOfOp "&&" = Just (ElmRightAssoc, ElmPrec3)
precOfOp "||" = Just (ElmRightAssoc, ElmPrec2)
precOfOp "|>" = Just (ElmLeftAssoc,  ElmPrec0)
precOfOp "<|" = Just (ElmRightAssoc, ElmPrec0)
precOfOp _    = Nothing

data ElmIndent
  = ElmNoIndent
  | ElmIndentBy Int ElmIndent
  | ElmIndentToMul Int ElmIndent

data ElmType
  = ElmTypeIdent Text
  | ElmFuncType ElmType ElmType
  | ElmTupleType [ElmType]
  | ElmRecordType [(Text, ElmType)]
  | ElmTypeFuncAppl Text [ElmType]

instance IsString ElmType where
  fromString = ElmTypeIdent . fromString

data ElmPattern
  = ElmPatIdent Text
  | ElmPatInt Int
  | ElmPatFuncAppl Text [ElmPattern]
  | ElmTuplePat [ElmPattern]
  | ElmCons ElmPattern ElmPattern
  | ElmListPat [ElmPattern]
  | ElmStringPat Text

instance IsString ElmPattern where
  fromString = ElmPatIdent . fromString

instance Num ElmPattern where
  fromInteger = ElmPatInt . fromInteger
  (+)    = error "Num instance only for fromInteger"
  (*)    = error "Num instance only for fromInteger"
  abs    = error "Num instance only for fromInteger"
  signum = error "Num instance only for fromInteger"
  negate = error "Num instance only for fromInteger"

data ElmExpr
  = ElmExprIdent Text
  | ElmExprInt Int
  | ElmBinOp ElmExpr Text ElmExpr
  | ElmMonOp Text ElmExpr
  | ElmTernOp ElmExpr ElmExpr ElmExpr
  | ElmCaseExpr ElmExpr [(ElmPattern, ElmExpr)]
  | ElmLetIn ElmPattern ElmExpr ElmExpr
  | ElmMember ElmExpr Text
  | ElmStringExpr Text
  | ElmTupleExpr [ElmExpr]
  | ElmListExpr [ElmExpr]
  | ElmFuncAppl ElmExpr [ElmExpr]
  | ElmRecord [(Text, ElmExpr)]
  | ElmRecordUpdate Text [(Text, ElmExpr)]
  | ElmLambda [ElmPattern] ElmExpr
  | ElmParenExpr ElmExpr

instance IsString ElmExpr where
  fromString = ElmExprIdent . fromString

instance Num ElmExpr where
  fromInteger = ElmExprInt . fromInteger
  (+)    = error "Num instance only for fromInteger"
  (*)    = error "Num instance only for fromInteger"
  abs    = error "Num instance only for fromInteger"
  signum = error "Num instance only for fromInteger"
  negate = error "Num instance only for fromInteger"

data ElmStmt
  = ElmModule Text [Text]
  | ElmImport Text [Text]
  | ElmTypeSig Text ElmType
  | ElmDef ElmPattern ElmExpr
  | ElmTypeDef Text [(Text, [ElmType])]
  | ElmTypeAlias Text ElmType
  | ElmComment ElmStmt Text
  | ElmBlankLine
  | ElmStmts [ElmStmt]

-- MARK: PRETTY

widthOfIndent :: ElmIndent -> Int
widthOfIndent = widthOfIndent' 0
  where widthOfIndent' :: Int -> ElmIndent -> Int
        widthOfIndent' x  ElmNoIndent         = x
        widthOfIndent' x (ElmIndentBy y i)    = widthOfIndent' (x + y) i
        widthOfIndent' x (ElmIndentToMul k i) = widthOfIndent' (x + ((-x) `mod` k)) i

showIndent :: ElmIndent -> Text
showIndent = flip Text.replicate " " . widthOfIndent

addIndent :: (ElmIndent, Text) -> Text
addIndent (_, "") = ""
addIndent (i, t)  = showIndent i ++ t

class Pretty a where
  prettyLines :: a -> ([(ElmIndent, Text)], (ElmAssoc, ElmPrec))

paren :: Pretty a => (ElmAssoc, ElmPrec) -> a -> [(ElmIndent, Text)]
paren (a, p) x =
  let (ts, (a', p')) = prettyLines x in
  let 
    ts' = case ts of
      [(_, l)] -> [(ElmNoIndent, "(" ++ l ++ ")")]
      _        ->
        (++ [(ElmNoIndent, ")")])
        . mapHeadTail (second ("( " ++)) (first . ElmIndentBy $ 2)
        $ ts
  in
  case compare p p' of
    LT -> ts
    EQ -> if a == a' then ts else ts'
    GT -> ts'

prettyTuple :: Pretty a => [a] -> ([(ElmIndent, Text)], (ElmAssoc, ElmPrec))
prettyTuple xs =
  ( let lss = map (fst . prettyLines) xs in
    if any isPlural lss
    then
      (++ [(ElmNoIndent, ")")])
      . concat
      . mapHeadTail
          ( mapHeadTail
              (second ("( " ++))
              (first . ElmIndentBy $ 2)
          )
          ( mapHeadTail
              (second (", " ++))
              (first . ElmIndentBy $ 2)
          )
      $ lss
    else
      [(ElmNoIndent, "( " ++ (intercalate ", " . map snd . concat $ lss) ++ " )")]
  , (ElmNoAssoc, ElmPrecParen)
  )

prettyList :: Pretty a => [a] -> ([(ElmIndent, Text)], (ElmAssoc, ElmPrec))
prettyList xs =
  ( let lss = map (fst . prettyLines) xs in
    if any isPlural lss
    then
      (++ [(ElmNoIndent, "]")])
      . concat
      . mapHeadTail
        ( mapHeadTail
            (second ("[ " ++))
            (first . ElmIndentBy $ 2)
        )
        ( mapHeadTail
            (second (", " ++))
            (first . ElmIndentBy $ 2)
        )
      $ lss
    else
      [(ElmNoIndent, "[ " ++ (intercalate ", " . map snd . concat $ lss) ++ " ]")]
  , (ElmNoAssoc, ElmPrecParen)
  )

instance Pretty ElmType where
  prettyLines (ElmTypeIdent i)       = ([(ElmNoIndent, i)], (ElmNoAssoc, ElmPrecLit))
  prettyLines (ElmFuncType t1 t2)    =
    ( case
        ( paren (ElmNoAssoc,    ElmPrec9) t1
        , paren (ElmRightAssoc, ElmPrec9) t2
        )
      of
        ([l1], [l2]) ->
          [(ElmNoIndent, snd l1 ++ " -> " ++ snd l2)]
        (ls1,  ls2)  ->
          (ls1 ++)
          . mapHeadTail
            (second ("-> " ++))
            (first . ElmIndentBy $ 4)
          $ ls2
    , (ElmRightAssoc, ElmPrec9)
    )
  prettyLines (ElmTupleType ts)      = prettyTuple ts
  prettyLines (ElmRecordType fs)     =
    ( (++ [(ElmNoIndent, "}")])
      . concat
      . mapHeadTail
        ( mapHeadTail
          (second ("{ " ++))
          (first (ElmIndentBy 4 . ElmIndentToMul 4))
        )
        ( mapHeadTail
          (second (", " ++))
          (first (ElmIndentBy 4 . ElmIndentToMul 4))
        )
      . map
        (\(i, t) -> case fst . prettyLines $ t of
          [(_, l)] -> [(ElmNoIndent, i ++ " : " ++ l)]
          ls       -> (ElmNoIndent, i ++ " :") : ls
        )
      $ fs
    , (ElmNoAssoc, ElmPrecParen)
    )
  prettyLines (ElmTypeFuncAppl i as) =
    ( let lss = map (paren (ElmNoAssoc, ElmPrecFuncAppl)) as in
      if any isPlural lss
      then ((ElmNoIndent, i) :) . concatMap (map (first . ElmIndentBy $ 4)) $ lss
      else [(ElmNoIndent, unwords . (i :) . map snd . concat $ lss)]
    , (ElmLeftAssoc, ElmPrecFuncAppl)
    )

instance Pretty ElmPattern where
  prettyLines (ElmPatIdent i)       =
    ( [(ElmNoIndent, i)]
    , (ElmNoAssoc, ElmPrecLit)
    )
  prettyLines (ElmPatInt n)         =
    ( [(ElmNoIndent, tshow n)]
    , (ElmNoAssoc, ElmPrecLit)
    )
  prettyLines (ElmPatFuncAppl i as) =
    ( let lss = map (paren (ElmNoAssoc, ElmPrecFuncAppl)) as in
      if any isPlural lss
      then ((ElmNoIndent, i) :) . concatMap (map (first . ElmIndentBy $ 4)) $ lss
      else [(ElmNoIndent, unwords . (i :) . map snd . concat $ lss)]
    , (ElmLeftAssoc, ElmPrecFuncAppl)
    )
  prettyLines (ElmTuplePat ps)      = prettyTuple ps
  prettyLines (ElmCons p1 p2)       =
    ( case
        ( paren (ElmNoAssoc,    ElmPrec5) p1
        , paren (ElmRightAssoc, ElmPrec5) p2
        )
      of
        ([l1], [l2]) -> [(ElmNoIndent, snd l1 ++ " :: " ++ snd l2)]
        (ls1,  ls2)  -> ls1 ++ mapHeadTail (second (":: " ++)) (first . ElmIndentBy $ 4) ls2
    , (ElmRightAssoc, ElmPrec5)
    )
  prettyLines (ElmListPat ps)       = prettyList ps
  prettyLines (ElmStringPat s)      = ([(ElmNoIndent, tshow s)], (ElmNoAssoc, ElmPrecLit))

instance Pretty ElmExpr where
  prettyLines (ElmExprIdent i)       =
    ( [(ElmNoIndent, i)]
    , (ElmNoAssoc, ElmPrecLit)
    )
  prettyLines (ElmExprInt n)         =
    ( [(ElmNoIndent, tshow n)]
    , (ElmNoAssoc, ElmPrecLit)
    )
  prettyLines (ElmBinOp e1 o e2)     =
    let (a, p) = (fmap fst &&& fmap snd) . precOfOp $ o in
    ( case
        ( paren (if a == Just ElmLeftAssoc  then ElmLeftAssoc  else ElmNoAssoc, fromMaybe ElmPrec9 p) e1
        , paren (if a == Just ElmRightAssoc then ElmRightAssoc else ElmNoAssoc, fromMaybe ElmPrec9 p) e2
        )
      of
        ([l1], [l2]) -> [(ElmNoIndent, snd l1 ++ " " ++ o ++ " " ++ snd l2)]
        (ls1,  ls2)  -> ls1 ++ mapHeadTail (ElmIndentBy 4 *** ((o ++ " ") ++)) (first . ElmIndentBy $ 4) ls2
    , (fromMaybe ElmNoAssoc a, fromMaybe ElmPrec0 p)
    )
  prettyLines (ElmMonOp o e)         =
    ( mapHead (second (o ++))
      . paren (ElmNoAssoc, ElmPrecFuncAppl)
      $ e
    , (ElmNoAssoc, ElmPrecFuncAppl)
    )
  prettyLines (ElmTernOp c e1 e2)    =
    ( ( case fst . prettyLines $ c of
          [(_, l)] -> [(ElmNoIndent, "if " ++ l ++ " then")]
          ls       ->
            [(ElmNoIndent, "if")]
            ++ map (first . ElmIndentBy $ 4) ls
            ++ [(ElmNoIndent, "then")]
      )
      ++ (map (first . ElmIndentBy $ 4) . fst . prettyLines $ e1)
      ++ [(ElmNoIndent, "")]
      ++ [(ElmNoIndent, "else")]
      ++ (map (first . ElmIndentBy $ 4) . fst . prettyLines $ e2)
    , (ElmNoAssoc, ElmPrecNone)
    )
  prettyLines (ElmCaseExpr c ps)     =
    ( ( case fst . prettyLines $ c of
          [(_, l)] -> [(ElmNoIndent, "case " ++ l ++ " of")]
          ls       -> [(ElmNoIndent, "case")] ++ map (first . ElmIndentBy $ 4) ls ++ [(ElmNoIndent, "of")]
      ) ++
      ( intercalate [(ElmNoIndent, "")]
      . map
        (\(p, e) -> 
          ( let ls = map (first . ElmIndentBy $ 4) . fst . prettyLines $ p in
            case ls of
              [l] -> [second (++ " ->") l]
              _   -> ls ++ [(ElmIndentBy 4 ElmNoIndent, "->")]
          )
          ++ (map (first . ElmIndentBy $ 8) . fst . prettyLines $ e)
        )
      $ ps
      )
    , (ElmNoAssoc, ElmPrecNone)
    )
  prettyLines (ElmLetIn p e1 e2)     =
    ( [(ElmNoIndent, "let")]
      ++ (map (first . ElmIndentBy $ 4) . mapLast (second (++ " =")) . fst . prettyLines $ p)
      ++ (map (first . ElmIndentBy $ 8) . fst . prettyLines $ e1)
      ++ [(ElmNoIndent, "in")]
      ++ (fst . prettyLines $ e2)
    , (ElmNoAssoc, ElmPrecNone)
    )
  prettyLines (ElmMember e i)        =
    ( case paren (ElmLeftAssoc, ElmPrecMember) e of
        [l] -> [second (++ ("." ++ i)) l]
        ls  -> ls ++ [(ElmIndentBy 4 ElmNoIndent, "." ++ i)]
    , (ElmLeftAssoc, ElmPrecMember)
    )
  prettyLines (ElmStringExpr s)      =
    ( [(ElmNoIndent, tshow s)]
    , (ElmNoAssoc, ElmPrecLit)
    )
  prettyLines (ElmTupleExpr es)      = prettyTuple es
  prettyLines (ElmListExpr es)       = prettyList es
  prettyLines (ElmFuncAppl e es)     =
    ( let ls = paren (ElmNoAssoc, ElmPrecFuncAppl) e in
      let lss = map (paren (ElmNoAssoc, ElmPrecFuncAppl)) es in
      if any isPlural (ls:lss)
      then
        (ls ++)
        . concatMap (map . first . ElmIndentBy $ 4)
        $ lss
      else
        [(ElmNoIndent, unwords . map snd . concat . (ls :) $ lss)]
    , (ElmLeftAssoc, ElmPrecFuncAppl)
    )
  prettyLines (ElmRecord fs)         =
    ( (++ [(ElmNoIndent, "}")])
      . concat
      . mapHeadTail
        ( mapHeadTail
          (second ("{ " ++))
          (first (ElmIndentBy 4 . ElmIndentToMul 4))
        )
        ( mapHeadTail
          (second (", " ++))
          (first (ElmIndentBy 4 . ElmIndentToMul 4))
        )
      . map
        (\(i, e) -> case fst . prettyLines $ e of
          [(_, l)] -> [(ElmNoIndent, i ++ " = " ++ l)]
          ls       -> (ElmNoIndent, i ++ " =") : ls
        )
      $ fs
    , (ElmNoAssoc, ElmPrecParen)
    )
  prettyLines (ElmRecordUpdate i []) =
    ( [(ElmNoIndent, i)]
    , (ElmNoAssoc, ElmPrecLit)
    )
  prettyLines (ElmRecordUpdate i fs) =
    ( ([(ElmNoIndent, "{ " ++ i)] ++)
      . (++ [(ElmNoIndent, "}")])
      . concat
      . mapHeadTail
        ( mapHeadTail
          (ElmIndentBy 4 *** ("| " ++))
          (first . ElmIndentBy $ 8)
        )
        ( mapHeadTail
          (ElmIndentBy 4 *** (", " ++))
          (first . ElmIndentBy $ 8)
        )
      . map
        (\(f, e) -> case fst . prettyLines $ e of
          [(_, l)] -> [(ElmNoIndent, f ++ " = " ++ l)]
          ls       -> (ElmNoIndent, f ++ " =") : ls
        )
      $ fs
    , (ElmNoAssoc, ElmPrecParen)
    )
  prettyLines (ElmLambda ps e)       =
    ( ( let lss = map (paren (ElmNoAssoc, ElmPrecFuncAppl)) ps in
        if any isPlural lss
        then mapHeadTail (second ("(\\" ++)) (first . ElmIndentBy $ 2) . concat $ lss
        else [(ElmNoIndent, "(\\" ++ (unwords . map snd . concat $ lss) ++ " ->")]
      )
      ++ (map (first . ElmIndentBy $ 4) . fst . prettyLines $ e)
      ++ [(ElmNoIndent, ")")]
    , (ElmNoAssoc, ElmPrecParen)
    )
  prettyLines (ElmParenExpr e)       =
    ( paren (ElmNoAssoc, ElmPrecLit) e
    , (ElmNoAssoc, ElmPrecParen)
    )

pretty :: ElmStmt -> Text
pretty (ElmModule m [])   = "module " ++ m
pretty (ElmModule m is)   = "module " ++ m ++ " exposing (" ++ intercalate ", " is ++ ")"
pretty (ElmImport m [])   = "import " ++ m
pretty (ElmImport m is)   = "import " ++ m ++ " exposing (" ++ intercalate ", " is ++ ")"
pretty (ElmTypeSig i t)   =
  case fst . prettyLines $ t of
    [(_, l)] -> i ++ " : " ++ l
    ls       ->
      intercalate "\n"
      . ((i ++ " :") :)
      . map ((++ "\n") . addIndent)
      $ ls
pretty (ElmDef p e)       =
  intercalate "\n"
  . map addIndent
  $ (mapLast (second (++ " =")) . fst . prettyLines $ p)
  ++ (map (first . ElmIndentBy $ 4) . fst . prettyLines $ e)
pretty (ElmTypeDef i cs)  =
  intercalate "\n"
  . (("type " ++ i) :)
  . map addIndent
  . concatMap (\(o, (c, ts)) ->
      let lss = map (paren (ElmNoAssoc, ElmPrecFuncAppl)) ts in
      map (first . ElmIndentBy $ 4)
      . mapHeadTail (second ((o ++ " ") ++)) (first . ElmIndentBy $ 4)
      $ if any isPlural lss
        then (ElmNoIndent, c) : concat lss
        else [(ElmNoIndent, unwords . (c :) . map snd . concat $ lss)]
    )
  . mapHeadTail ("=",) ("|",)
  $ cs
pretty (ElmTypeAlias i t) =
  intercalate "\n"
  . (("type alias " ++ i ++ " =") :)
  . map (addIndent . first (ElmIndentBy 4))
  . fst
  . prettyLines
  $ t
pretty (ElmComment s c)   =
  let ls = lines . pretty $ s in
  let n = maximum . ncons 0 . map length $ ls in
  intercalate "\n"
  . map (\l -> l ++ Text.replicate (n - length l) " " ++ " -- " ++ c)
  $ ls
pretty  ElmBlankLine      = ""
pretty (ElmStmts ss)      = intercalate "\n" . map pretty $ ss

