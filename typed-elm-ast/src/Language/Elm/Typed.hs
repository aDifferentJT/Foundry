{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, NoImplicitPrelude, OverloadedStrings, PartialTypeSignatures, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

{-|
Module      : Language.Elm.Typed
Description : A typed Elm AST
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Language.Elm.Typed
  ( ($$)
  , elmImport
  , elmDef
  ) where

import ClassyPrelude

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import qualified Data.Set as Set
import Data.Proxy (Proxy(..))
import GHC.Types (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Elm.AST

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

class ElmExprRep (t :: ElmTypeKind) a | a -> t where
  elmExpr :: a -> ElmExpr

newtype ElmIdentRep (t :: ElmTypeKind) = ElmIdentRep Text

instance ElmExprRep t (ElmIdentRep t) where
  elmExpr (ElmIdentRep i) = ElmExprIdent i

instance ElmExprRep ('ElmTypeIdent "Int") Int where
  elmExpr = ElmExprInt

instance ElmExprRep ('ElmTypeIdent "Int") Text where
  elmExpr = ElmStringExpr

instance (ElmExprRep t1 a1, ElmExprRep t2 a2) => ElmExprRep ('ElmTupleType '[t1, t2]) (a1, a2) where
  elmExpr (x, y) = ElmTupleExpr [elmExpr x, elmExpr y]

instance (ElmExprRep t1 a1, ElmExprRep t2 a2, ElmExprRep t3 a3) => ElmExprRep ('ElmTupleType '[t1, t2, t3]) (a1, a2, a3) where
  elmExpr (x, y, z) = ElmTupleExpr [elmExpr x, elmExpr y, elmExpr z]

instance ElmExprRep t a => ElmExprRep ('ElmTypeFuncAppl "List" '[t]) [a] where
  elmExpr = ElmListExpr . map elmExpr

data ElmFuncApplRep t2 a b where
  ElmFuncApplRep :: (ElmExprRep ('ElmFuncType t1 t2) a, ElmExprRep t1 b) => a -> b -> ElmFuncApplRep t2 a b

instance (ElmExprRep ('ElmFuncType t1 t2) a, ElmExprRep t1 b) => ElmExprRep t2 (ElmFuncApplRep t2 a b) where
  elmExpr (ElmFuncApplRep f x) = ElmFuncAppl (elmExpr f) [elmExpr x]

($$) :: (ElmExprRep ('ElmFuncType t1 t2) a, ElmExprRep t1 b) => a -> b -> ElmFuncApplRep t2 a b
($$) = ElmFuncApplRep

data ElmLambdaRep (t1 :: ElmTypeKind) b = ElmLambdaRep Text (forall a. ElmExprRep t1 a => a -> b)

instance ElmExprRep t2 b => ElmExprRep ('ElmFuncType t1 t2) (ElmLambdaRep t1 b) where
  elmExpr (ElmLambdaRep i f) = ElmLambda [ElmPatIdent i] . elmExpr . f . ElmIdentRep $ i

type ElmStmtMonad a = State (Set ElmStmt) a

newtype UnknownElmIdentRep = UnknownElmIdentRep (forall (t :: ElmTypeKind). ElmIdentRep t)

mkUnknownElmIdentRep :: Text -> UnknownElmIdentRep
mkUnknownElmIdentRep i = UnknownElmIdentRep i'
  where i' :: forall (t :: ElmTypeKind). ElmIdentRep t
        i' = ElmIdentRep i

elmImport :: Text -> [Text] -> ElmStmtMonad (Text -> UnknownElmIdentRep, [UnknownElmIdentRep])
elmImport m is = do
  State.modify (Set.insert (ElmImport m is))
  return 
    ( mkUnknownElmIdentRep . ((m ++ ".") ++)
    , map mkUnknownElmIdentRep is
    )

type family NestedPairs (f :: k -> *) (ks :: [k]) :: *
type instance NestedPairs _ '[]       = ()
type instance NestedPairs f (k ': ks) = (f k, NestedPairs f ks)

type family NestedPairsConst (x :: *) (ks :: [k]) :: *
type instance NestedPairsConst _ '[]       = ()
type instance NestedPairsConst x (_ ': ks) = (x, NestedPairsConst x ks)

elmDef :: forall t a ts. (ElmExprRep t a, ElmTypeFromKind t, ElmTypesFromKinds ts) => Text -> NestedPairsConst Text ts -> (NestedPairs ElmIdentRep ts -> a) -> ElmStmtMonad (ElmIdentRep t)
elmDef ident args body = do
  State.modify
    ( Set.insert
      ( ElmStmts
        [ ElmTypeSig ident (foldr ElmFuncType (elmTypeFromKind (Proxy :: Proxy t)) (elmTypesFromKinds (Proxy :: Proxy ts)))
        ]
      )
    )
  let _ = args
  let _ = body
  return . ElmIdentRep $ ident

