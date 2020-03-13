{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, NoImplicitPrelude, OverloadedStrings, PartialTypeSignatures, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilyDependencies, TypeOperators, UndecidableInstances #-}

{-|
Module      : Language.Elm.Typed
Description : A typed Elm AST
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.Elm.Typed
  ( HList(..)
  , ElmTypeKind
  , ElmIdentRep
  , ElmStmtMonad
  , ($$)
  , (.:.)
  , elmModule
  , elmImport
  , elmDef
  , elmTypeDef
  ) where

import ClassyPrelude

import Control.Monad.Trans.State (State, runState)
import qualified Control.Monad.Trans.State as State
import qualified Data.Set as Set
import Data.Proxy (Proxy(..))
--import qualified GHC.Generics as Generics
import GHC.Types (Constraint, Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Elm.AST

type family MapConstTypeList (x :: k2) (ts :: [k1]) :: [k2]
type instance MapConstTypeList _ '[]       = '[]
type instance MapConstTypeList x (_ ': ts) = x ': MapConstTypeList x ts

type family MapTypeList (f :: k1 -> k2) (ts :: [k1]) :: [k2]
type instance MapTypeList _ '[]       = '[]
type instance MapTypeList f (t ': ts) = f t ': MapTypeList f ts

type family MapFstTypeList (ts :: [(k1, k2)]) :: [k1]
type instance MapFstTypeList '[]            = '[]
type instance MapFstTypeList ('(t, _) ': ts) = t ': MapFstTypeList ts

type family ZipTypeListsFunc (xs :: [*]) (ys :: [*]) = (zs :: [*]) | zs -> xs ys
type instance ZipTypeListsFunc '[]       '[]       = '[]
type instance ZipTypeListsFunc (x ': xs) (y ': ys) = (x -> y) ': ZipTypeListsFunc xs ys

type family ConcatTypeLists (xs :: [k]) (ys :: [k]) :: [k]
type instance ConcatTypeLists '[]       ys = ys
type instance ConcatTypeLists (x ': xs) ys = x ': ConcatTypeLists xs ys

class TypeListElem (x :: k) (ys :: [k])
instance TypeListElem x (x ': ys)
instance TypeListElem x ys => TypeListElem x (y ': ys)

class TypeListSubset (xs :: [k]) (ys :: [k])
instance TypeListSubset '[] ys
instance TypeListElem x ys => TypeListSubset (x ': xs) ys

class TypeListLookup (x :: k1) (xys :: [(k1, k2)]) (y :: k2) | x xys -> y
instance TypeListLookup x ('(x, y) ': xys) y
instance TypeListLookup x xys y => TypeListLookup x (xy ': xys) y

infixr 5 :::

data HList (ts :: [k]) where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)


class AllEqual t (ts :: [*]) where
  flattenHList :: HList ts -> [t]

instance AllEqual t '[] where
  flattenHList HNil = []

instance AllEqual t ts => AllEqual t (t ': ts) where
  flattenHList (x ::: xs) = x : flattenHList xs


class MapHList (as :: [*]) (bs :: [*]) where
  mapHList :: HList (ZipTypeListsFunc as bs) -> HList as -> HList bs

instance MapHList '[] '[] where
  mapHList HNil HNil = HNil

instance (MapHList as bs) => MapHList (a ': as) (b ': bs) where
  mapHList (f ::: fs) (x ::: xs) = f x ::: mapHList fs xs


class ReplicatePolyResultHList (a :: *) (c :: k -> *) (ts :: [k]) where
  replicatePolyResultHList :: Proxy ts -> (forall (t :: k). a -> c t) -> HList (ZipTypeListsFunc (MapConstTypeList a ts) (MapTypeList c ts))

instance ReplicatePolyResultHList a c '[] where
  replicatePolyResultHList Proxy _ = HNil

instance (ReplicatePolyResultHList a c ts) => ReplicatePolyResultHList a c (t ': ts) where
  replicatePolyResultHList Proxy x = (x :: a -> c t) ::: replicatePolyResultHList (Proxy :: Proxy ts) x


class ReplicateConstrainedPolyArgHList (c :: * -> Constraint) (as :: [*]) (b :: *) where
  replicateConstrainedPolyArgHList :: Proxy c -> Proxy as -> Proxy b -> (forall a. c a => a -> b) -> HList (ZipTypeListsFunc as (MapConstTypeList b as))

instance ReplicateConstrainedPolyArgHList c '[] b where
  replicateConstrainedPolyArgHList Proxy Proxy Proxy _ = HNil

instance (ReplicateConstrainedPolyArgHList c as b, c a) => ReplicateConstrainedPolyArgHList c (a ': as) b where
  replicateConstrainedPolyArgHList Proxy Proxy Proxy x = (x :: a -> b) ::: replicateConstrainedPolyArgHList (Proxy :: Proxy c) (Proxy :: Proxy as) (Proxy :: Proxy b) x


class ReplicateDoublePolyArgHList (c1 :: k1 -> *) (c2 :: k2 -> k1) (ts :: [k2]) (a :: *) where
  replicateDoublePolyArgHList :: Proxy ts -> (forall (t :: k2). c1 (c2 t) -> a) -> HList (ZipTypeListsFunc (MapTypeList c1 (MapTypeList c2 ts)) (MapConstTypeList a ts))

instance ReplicateDoublePolyArgHList c1 c2 '[] a where
  replicateDoublePolyArgHList Proxy _ = HNil

instance (ReplicateDoublePolyArgHList c1 c2 ts a) => ReplicateDoublePolyArgHList c1 c2 (t ': ts) a where
  replicateDoublePolyArgHList Proxy x = (x :: c1 (c2 t) -> a) ::: replicateDoublePolyArgHList (Proxy :: Proxy ts) x


type ElmTypeKind = ElmTypeWithIdent Symbol

class ElmTypeFromKind (t :: ElmTypeKind) where
  elmTypeFromKind :: Proxy t -> ElmType

instance KnownSymbol i => ElmTypeFromKind ('ElmTypeIdent i) where
  elmTypeFromKind Proxy = ElmTypeIdent . pack . symbolVal $ (Proxy :: Proxy i)

instance (ElmTypeFromKind t1, ElmTypeFromKind t2) => ElmTypeFromKind ('ElmFuncType t1 t2) where
  elmTypeFromKind Proxy = ElmFuncType (elmTypeFromKind (Proxy :: Proxy t1)) (elmTypeFromKind (Proxy :: Proxy t2))

class ElmTypesFromKinds (ts :: [ElmTypeKind]) where
  elmTypesFromKinds :: Proxy ts -> [ElmType]
instance ElmTypesFromKinds '[] where
  elmTypesFromKinds Proxy = []
instance (ElmTypeFromKind t, ElmTypesFromKinds ts) => ElmTypesFromKinds (t ': ts) where
  elmTypesFromKinds Proxy = elmTypeFromKind (Proxy :: Proxy t) : elmTypesFromKinds (Proxy :: Proxy ts)

instance ElmTypesFromKinds ts => ElmTypeFromKind ('ElmTupleType ts) where
  elmTypeFromKind Proxy = ElmTupleType . elmTypesFromKinds $ (Proxy :: Proxy ts)

class RecordEntrysFromKinds (es :: [(Symbol, ElmTypeKind)]) where
  recordEntrysFromKinds :: Proxy es -> [(Text, ElmType)]
instance RecordEntrysFromKinds '[] where
  recordEntrysFromKinds Proxy = []
instance (KnownSymbol i, ElmTypeFromKind t, RecordEntrysFromKinds es) => RecordEntrysFromKinds ('(i, t) ': es) where
  recordEntrysFromKinds Proxy = (pack . symbolVal $ (Proxy :: Proxy i), elmTypeFromKind (Proxy :: Proxy t)) : recordEntrysFromKinds (Proxy :: Proxy es)

instance RecordEntrysFromKinds es => ElmTypeFromKind ('ElmRecordType es) where
  elmTypeFromKind Proxy = ElmRecordType . recordEntrysFromKinds $ (Proxy :: Proxy es)

instance (KnownSymbol f, ElmTypesFromKinds ts) => ElmTypeFromKind ('ElmTypeFuncAppl f ts) where
  elmTypeFromKind Proxy = ElmTypeFuncAppl (pack . symbolVal $ (Proxy :: Proxy f)) . elmTypesFromKinds $ (Proxy :: Proxy ts)


newtype ElmIdentRep (t :: ElmTypeKind) = ElmIdentRep Text


class ElmPatternRep a where
  type ElmPatTypeKind a :: ElmTypeKind
  type ElmPatBoundType a :: *
  elmPattern :: a -> ElmPattern
  elmPatBound :: a -> ElmPatBoundType a


instance ElmPatternRep (ElmIdentRep t) where
  type ElmPatTypeKind (ElmIdentRep t) = t
  type ElmPatBoundType (ElmIdentRep t) = ()
  elmPattern (ElmIdentRep i) = ElmPatIdent i
  elmPatBound = const ()


instance ElmPatternRep Int where
  type ElmPatTypeKind Int = 'ElmTypeIdent "Int"
  type ElmPatBoundType Int = ()
  elmPattern = ElmPatInt
  elmPatBound = const ()


class CanMapTypeListElmPatBoundType (as :: [*]) where
  type MapTypeListElmPatBoundType (as :: [*]) :: [*]
  mapTypeListElmPatBoundType :: HList as -> HList (MapTypeListElmPatBoundType as)
instance CanMapTypeListElmPatBoundType '[] where
  type MapTypeListElmPatBoundType '[]       = '[]
  mapTypeListElmPatBoundType HNil = HNil
instance (ElmPatternRep a, CanMapTypeListElmPatBoundType as) => CanMapTypeListElmPatBoundType (a ': as) where
  type MapTypeListElmPatBoundType (a ': as) = ElmPatBoundType a ': MapTypeListElmPatBoundType as
  mapTypeListElmPatBoundType (x ::: xs) = elmPatBound x ::: mapTypeListElmPatBoundType xs

data ElmPatFuncApplRep (as :: [*]) (t :: ElmTypeKind) = ElmPatFuncApplRep Text (HList as)
instance
    ( AllEqual ElmPattern (MapConstTypeList ElmPattern as)
    , MapHList as (MapConstTypeList ElmPattern as)
    , ReplicateConstrainedPolyArgHList ElmPatternRep as ElmPattern
    , CanMapTypeListElmPatBoundType as
    ) => ElmPatternRep (ElmPatFuncApplRep as t) where
  type ElmPatTypeKind (ElmPatFuncApplRep as t) = t
  type ElmPatBoundType (ElmPatFuncApplRep as t) = HList (MapTypeListElmPatBoundType as)
  elmPattern (ElmPatFuncApplRep i xs) = ElmPatFuncAppl i . flattenHList $ mapHList
    ( replicateConstrainedPolyArgHList
      (Proxy :: Proxy ElmPatternRep)
      (Proxy :: Proxy as)
      (Proxy :: Proxy ElmPattern)
      (elmPattern :: forall a. ElmPatternRep a => a -> ElmPattern)
    )
    xs
  elmPatBound (ElmPatFuncApplRep _ xs) = mapTypeListElmPatBoundType xs


instance (ElmPatternRep a, ElmPatternRep b) => ElmPatternRep (a, b) where
  type ElmPatTypeKind (a, b) = 'ElmTupleType '[ElmPatTypeKind a, ElmPatTypeKind b]
  type ElmPatBoundType (a, b) = (ElmPatBoundType a, ElmPatBoundType b)
  elmPattern (x, y) = ElmTuplePat [elmPattern x, elmPattern y]
  elmPatBound (x, y) = (elmPatBound x, elmPatBound y)


instance (ElmPatternRep a, ElmPatternRep b, ElmPatternRep c) => ElmPatternRep (a, b, c) where
  type ElmPatTypeKind (a, b, c) = 'ElmTupleType '[ElmPatTypeKind a, ElmPatTypeKind b, ElmPatTypeKind c]
  type ElmPatBoundType (a, b, c) = (ElmPatBoundType a, ElmPatBoundType b, ElmPatBoundType c)
  elmPattern (x, y, z) = ElmTuplePat [elmPattern x, elmPattern y, elmPattern z]
  elmPatBound (x, y, z) = (elmPatBound x, elmPatBound y, elmPatBound z)


data (:-:) a b where
  (:-:) :: (ElmPatternRep a, ElmPatternRep b, 'ElmTypeFuncAppl "List" '[ElmPatTypeKind a] ~ ElmPatTypeKind b) => a -> b -> a :-: b
instance ElmPatternRep (a :-: b) where
  type ElmPatTypeKind (a :-: b) = ElmPatTypeKind b
  type ElmPatBoundType (a :-: b) = (ElmPatBoundType a, ElmPatBoundType b)
  elmPattern (x :-: xs) = ElmCons (elmPattern x) (elmPattern xs)
  elmPatBound (x :-: y) = (elmPatBound x, elmPatBound y)


instance ElmPatternRep a => ElmPatternRep [a] where
  type ElmPatTypeKind [a] = 'ElmTypeFuncAppl "List" '[ElmPatTypeKind a]
  type ElmPatBoundType [a] = [ElmPatBoundType a]
  elmPattern = ElmListPat . map elmPattern
  elmPatBound = map elmPatBound


instance ElmPatternRep Text where
  type ElmPatTypeKind Text = 'ElmTypeIdent "String"
  type ElmPatBoundType Text = ()
  elmPattern = ElmStringPat
  elmPatBound = const ()


class ElmExprRep a where
  type ElmExprTypeKind a :: ElmTypeKind

  elmTypeDef :: Proxy a -> Maybe ElmStmt
  --default elmTypeDef :: (Generic a, GenericGetElmTypeDef (Generics.Rep a)) => Proxy a -> Maybe ElmStmt
  --elmTypeDef Proxy = Just (genericGetElmTypeDef (Proxy :: Proxy (Generics.Rep a)))

  elmExpr :: a -> ElmExpr

elmTypeFromExprRep :: forall a. (ElmExprRep a, ElmTypeFromKind (ElmExprTypeKind a)) => Proxy a -> ElmType
elmTypeFromExprRep Proxy = elmTypeFromKind (Proxy :: Proxy (ElmExprTypeKind a))

instance ElmExprRep (ElmIdentRep t) where
  type ElmExprTypeKind (ElmIdentRep t) = t
  elmTypeDef Proxy = Nothing
  elmExpr (ElmIdentRep i) = ElmExprIdent i


instance ElmExprRep Int where
  type ElmExprTypeKind Int = 'ElmTypeIdent "Int"
  elmTypeDef Proxy = Nothing
  elmExpr = ElmExprInt


class ElmCases (t1 :: ElmTypeKind) (cs :: [*]) (t2 :: ElmTypeKind) where
  elmCases :: Proxy t1 -> Proxy t2 -> HList cs -> [(ElmPattern, ElmExpr)]
instance ElmCases t1 '[] t2 where
  elmCases Proxy Proxy HNil = []
instance (ElmCases t1 cs t2, ElmPatternRep p, ElmPatTypeKind p ~ t1, ElmExprRep e, ElmExprTypeKind e ~ t2) => ElmCases t1 ((p, e) ': cs) t2 where
  elmCases Proxy Proxy ((p, e) ::: cs) = (elmPattern p, elmExpr e) : elmCases (Proxy :: Proxy t1) (Proxy :: Proxy t2) cs

data ElmCaseExprRep a (bs :: [*]) t where
  ElmCaseExprRep :: (ElmExprRep a, ElmCases (ElmExprTypeKind a) bs t) => a -> HList bs -> ElmCaseExprRep a bs t

instance ElmExprRep (ElmCaseExprRep a bs t) where
  type ElmExprTypeKind (ElmCaseExprRep a bs t) = t
  elmTypeDef Proxy = Nothing
  elmExpr (ElmCaseExprRep e cs) = ElmCaseExpr (elmExpr e) (elmCases (Proxy :: Proxy (ElmExprTypeKind a)) (Proxy :: Proxy t) cs)


data ElmLetInRep a b where
  ElmLetInRep :: (ElmExprRep a, ElmExprRep b) => Text -> a -> (ElmIdentRep (ElmExprTypeKind a) -> b) -> ElmLetInRep a b

instance ElmExprRep (ElmLetInRep a b) where
  type ElmExprTypeKind (ElmLetInRep a b) = ElmExprTypeKind b
  elmTypeDef Proxy = Nothing
  elmExpr (ElmLetInRep i x f) = ElmLetIn (ElmPatIdent i) (elmExpr x) . elmExpr . f . ElmIdentRep $ i


instance ElmExprRep Text where
  type ElmExprTypeKind Text = 'ElmTypeIdent "String"
  elmTypeDef Proxy = Nothing
  elmExpr = ElmStringExpr


instance (ElmExprRep a, ElmExprRep b) => ElmExprRep (a, b) where
  type ElmExprTypeKind (a, b) = 'ElmTupleType '[ElmExprTypeKind a, ElmExprTypeKind b]
  elmTypeDef Proxy = Nothing
  elmExpr (x, y) = ElmTupleExpr [elmExpr x, elmExpr y]


instance (ElmExprRep a, ElmExprRep b, ElmExprRep c) => ElmExprRep (a, b, c) where
  type ElmExprTypeKind (a, b, c) = 'ElmTupleType '[ElmExprTypeKind a, ElmExprTypeKind b, ElmExprTypeKind c]
  elmTypeDef Proxy = Nothing
  elmExpr (x, y, z) = ElmTupleExpr [elmExpr x, elmExpr y, elmExpr z]


instance ElmExprRep a => ElmExprRep [a] where
  type ElmExprTypeKind [a] = 'ElmTypeFuncAppl "List" '[ElmExprTypeKind a]
  elmTypeDef Proxy = Nothing
  elmExpr = ElmListExpr . map elmExpr


type family ElmFuncReturnType (t :: ElmTypeKind) :: ElmTypeKind
type instance ElmFuncReturnType ('ElmFuncType _ t) = t

data ElmFuncApplRep a b where
  ElmFuncApplRep :: (ElmExprRep a, ElmExprRep b) => a -> b -> ElmFuncApplRep a b

instance (ElmExprRep a, ElmExprRep b, ElmExprTypeKind a ~ 'ElmFuncType (ElmExprTypeKind b) r) => ElmExprRep (ElmFuncApplRep a b) where
  type ElmExprTypeKind (ElmFuncApplRep a b) = ElmFuncReturnType (ElmExprTypeKind a)
  elmTypeDef Proxy = Nothing
  elmExpr (ElmFuncApplRep f x) = ElmFuncAppl (elmExpr f) [elmExpr x]

($$) :: (ElmExprRep a, ElmExprRep b) => a -> b -> ElmFuncApplRep a b
($$) = ElmFuncApplRep


class ElmRecordFieldList (fs :: [(Symbol, *)]) where
  type ElmRecordFieldTypes (fs :: [(Symbol, *)]) :: [*]
  type ElmRecordFieldKinds (fs :: [(Symbol, *)]) :: [(Symbol, ElmTypeKind)]
  elmRecordFieldExprs :: Proxy fs -> HList (ElmRecordFieldTypes fs) -> [(Text, ElmExpr)]

instance ElmRecordFieldList '[] where
  type ElmRecordFieldTypes '[] = '[]
  type ElmRecordFieldKinds '[] = '[]
  elmRecordFieldExprs Proxy HNil = []

instance (KnownSymbol n, ElmExprRep t, ElmRecordFieldList fs) => ElmRecordFieldList ('(n, t) ': fs) where
  type ElmRecordFieldTypes ('(n, t) ': fs) = t ': ElmRecordFieldTypes fs
  type ElmRecordFieldKinds ('(n, t) ': fs) = '(n, ElmExprTypeKind t) ': ElmRecordFieldKinds fs
  elmRecordFieldExprs Proxy (x ::: xs) = (pack . symbolVal $ (Proxy :: Proxy n), elmExpr x) : elmRecordFieldExprs (Proxy :: Proxy fs) xs

data ElmRecordRep (fs :: [(Symbol, *)]) where
  ElmRecordRep :: ElmRecordFieldList fs => HList (ElmRecordFieldTypes fs) -> ElmRecordRep fs

instance ElmRecordFieldList fs => ElmExprRep (ElmRecordRep fs) where
  type ElmExprTypeKind (ElmRecordRep fs) = 'ElmRecordType (ElmRecordFieldKinds fs)
  elmTypeDef Proxy = Nothing
  elmExpr (ElmRecordRep xs) = ElmRecord . elmRecordFieldExprs (Proxy :: Proxy fs) $ xs


type family ElmRecordFields (r :: ElmTypeKind) :: [(Symbol, ElmTypeKind)]
type instance ElmRecordFields ('ElmRecordType fs) = fs

data ElmMemberRep a n t where
  ElmMemberRep :: (ElmExprRep a, TypeListLookup n (ElmRecordFields (ElmExprTypeKind a)) t, KnownSymbol n) => a -> Proxy n -> ElmMemberRep a n t

instance ElmExprRep (ElmMemberRep a n t) where
  type ElmExprTypeKind (ElmMemberRep a n t) = t
  elmTypeDef Proxy = Nothing
  elmExpr (ElmMemberRep x Proxy) = ElmMember (elmExpr x) (pack . symbolVal $ (Proxy :: Proxy n))

(.:.) :: (ElmExprRep a, TypeListLookup n (ElmRecordFields (ElmExprTypeKind a)) t, KnownSymbol n) => a -> Proxy n -> ElmMemberRep a n t
(.:.) = ElmMemberRep


data ElmRecordUpdateRep (fs' :: [(Symbol, ElmTypeKind)]) (fs :: [(Symbol, *)]) where
  ElmRecordUpdateRep ::
    ( ElmRecordFieldList fs
    , TypeListSubset (ElmRecordFieldKinds fs) fs'
    ) => ElmIdentRep ('ElmRecordType fs')
      -> Proxy fs
      -> HList (ElmRecordFieldTypes fs)
      -> ElmRecordUpdateRep a fs

instance ElmExprRep (ElmRecordUpdateRep fs' fs) where
  type ElmExprTypeKind (ElmRecordUpdateRep fs' fs) = 'ElmRecordType fs'
  elmTypeDef Proxy = Nothing
  elmExpr (ElmRecordUpdateRep (ElmIdentRep i) Proxy xs) = ElmRecordUpdate i . elmRecordFieldExprs (Proxy :: Proxy fs) $ xs


data ElmLambdaRep a b where
  ElmLambdaRep :: (ElmPatternRep a, ElmExprRep b) => a -> (ElmPatBoundType a -> b) -> ElmLambdaRep a b

instance ElmExprRep (ElmLambdaRep a b) where
  type ElmExprTypeKind (ElmLambdaRep a b) = 'ElmFuncType (ElmPatTypeKind a) (ElmExprTypeKind b)
  elmTypeDef Proxy = Nothing
  elmExpr (ElmLambdaRep p f) = ElmLambda [elmPattern p] . elmExpr . f . elmPatBound $ p


data AnyElmExpr (t :: ElmTypeKind) where
  AnyElmExpr :: (ElmExprRep a, ElmExprTypeKind a ~ t) => a -> AnyElmExpr t

instance ElmExprRep (AnyElmExpr t) where
  type ElmExprTypeKind (AnyElmExpr t) = t
  elmTypeDef Proxy = Nothing
  elmExpr (AnyElmExpr e) = elmExpr e


type ElmStmtMonad = State (Set ElmStmt)

elmModule :: forall (ts :: [ElmTypeKind]).
  ( AllEqual (ElmStmtMonad Text) (MapConstTypeList (ElmStmtMonad Text) ts)
  , MapHList (MapTypeList ElmStmtMonad (MapTypeList ElmIdentRep ts)) (MapConstTypeList (ElmStmtMonad Text) ts)
  , ReplicateDoublePolyArgHList ElmStmtMonad ElmIdentRep ts (ElmStmtMonad Text)
  ) => Text
    -> Proxy ts
    -> HList (MapTypeList ElmStmtMonad (MapTypeList ElmIdentRep ts))
    -> ElmStmt
elmModule name Proxy es = ElmStmts
  [ ElmModule name exportIdents
  , ElmBlankLine
  , ElmStmts . intersperse (ElmStmts . replicate 2 $ ElmBlankLine) . Set.toList $ stmts
  ]
  where exportIdents :: [Text]
        stmts :: Set ElmStmt
        (exportIdents, stmts) =
          flip runState Set.empty
          . (sequence :: [ElmStmtMonad Text] -> ElmStmtMonad [Text])
          . flattenHList
          . mapHList
            ( replicateDoublePolyArgHList
              (Proxy :: Proxy ts)
              ((fmap $ \(ElmIdentRep i) -> i) :: forall t. ElmStmtMonad (ElmIdentRep t) -> ElmStmtMonad Text)
            )
          $ (es :: HList (MapTypeList ElmStmtMonad (MapTypeList ElmIdentRep ts)))

newtype UnknownElmIdentRep = UnknownElmIdentRep (forall (t :: ElmTypeKind). ElmIdentRep t)

mkUnknownElmIdentRep :: Text -> UnknownElmIdentRep
mkUnknownElmIdentRep i = UnknownElmIdentRep i'
  where i' :: forall (t :: ElmTypeKind). ElmIdentRep t
        i' = ElmIdentRep i

elmImport :: forall (ts :: [ElmTypeKind]).
  ( AllEqual Text (MapConstTypeList Text ts)
  , MapHList (MapConstTypeList Text ts) (MapTypeList ElmIdentRep ts)
  , ReplicatePolyResultHList Text ElmIdentRep ts
  ) => Text
    -> Proxy ts
    -> HList (MapConstTypeList Text ts)
    -> ElmStmtMonad (Text -> UnknownElmIdentRep, HList (MapTypeList ElmIdentRep ts))
elmImport m Proxy is = do
  State.modify (Set.insert (ElmImport m (flattenHList is)))
  return 
    ( mkUnknownElmIdentRep . ((m ++ ".") ++)
    , mapHList (replicatePolyResultHList (Proxy :: Proxy ts) ElmIdentRep) is
    )

elmDef :: forall a (ts :: [ElmTypeKind]).
  ( ElmExprRep a
  , ElmTypeFromKind (ElmExprTypeKind a)
  , ElmTypesFromKinds ts
  , AllEqual Text (MapConstTypeList Text ts)
  , MapHList (MapConstTypeList Text ts) (MapTypeList ElmIdentRep ts)
  , ReplicatePolyResultHList Text ElmIdentRep ts
  ) => Text
    -> Proxy ts
    -> HList (MapConstTypeList Text ts)
    -> (HList (MapTypeList ElmIdentRep ts) -> ElmStmtMonad a)
    -> ElmStmtMonad (ElmIdentRep (ElmExprTypeKind a))
elmDef ident Proxy args body = do
  body' <- fmap elmExpr . body . mapHList (replicatePolyResultHList (Proxy :: Proxy ts) ElmIdentRep) $ args
  State.modify
    ( Set.insert
      ( ElmStmts
        [ ElmTypeSig ident (foldr ElmFuncType (elmTypeFromExprRep (Proxy :: Proxy a)) (elmTypesFromKinds (Proxy :: Proxy ts)))
        , ElmDef (ElmPatFuncAppl ident . map ElmPatIdent . flattenHList $ args) body'
        ]
      )
    )
  return . ElmIdentRep $ ident

{-
class GenericGetArguments (r :: p -> *) where
  genericGetArguments :: Proxy r -> [ElmType]
instance GenericGetArguments Generics.U1 where
  genericGetArguments Proxy = []
instance (ElmExprRep a, ElmTypeFromKind (ElmExprTypeKind a)) => GenericGetArguments (Generics.S1 m (Generics.Rec0 a)) where
  genericGetArguments Proxy = [elmTypeFromExprRep (Proxy :: Proxy a)]
instance (GenericGetArguments r1, GenericGetArguments r2) => GenericGetArguments (r1 Generics.:*: r2) where
  genericGetArguments Proxy = genericGetArguments (Proxy :: Proxy r1) ++ genericGetArguments (Proxy :: Proxy r2)

class GenericGetConstructors (r :: p -> *) where
  genericGetConstructors :: Proxy r -> [(Text, [ElmType])]
instance (KnownSymbol n, GenericGetArguments args) => GenericGetConstructors (Generics.C1 ('Generics.MetaCons n f s) args) where
  genericGetConstructors Proxy = [(pack . symbolVal $ (Proxy :: Proxy n), genericGetArguments (Proxy :: Proxy args))]
instance (GenericGetConstructors r1, GenericGetConstructors r2) => GenericGetConstructors (r1 Generics.:+: r2) where
  genericGetConstructors Proxy = genericGetConstructors (Proxy :: Proxy r1) ++ genericGetConstructors (Proxy :: Proxy r2)

class GenericGetElmTypeDef (r :: p -> *) where
  genericGetElmTypeDef :: Proxy r -> ElmStmt
instance (KnownSymbol n, GenericGetConstructors cs) => GenericGetElmTypeDef (Generics.D1 ('Generics.MetaData n m p nt) cs) where
  genericGetElmTypeDef Proxy = ElmTypeDef (pack . symbolVal $ (Proxy :: Proxy n)) (genericGetConstructors (Proxy :: Proxy cs))

type family GenericGetElmTypeKind (r :: p -> *) :: ElmTypeKind
type instance GenericGetElmTypeKind (Generics.D1 ('Generics.MetaData n m p nt) cs) = 'ElmTypeIdent n
-}

