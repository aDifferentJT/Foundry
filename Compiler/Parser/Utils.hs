{-# LANGUAGE LambdaCase #-}

module Utils
  ( Bit(..)
  , zipBy
  , zip3By
  ) where

import Data.List(sortBy)

import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read

data Bit = Zero | One
  deriving (Eq,Show)

instance Read Bit where
  readPrec = ReadPrec.get >>= \case
    '0' -> return Zero
    '1' -> return One
    _   -> ReadPrec.pfail

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))

zipBy :: Ord c => (a -> c) -> (b -> c) -> [a] -> [b] -> ([(a, b)], [a], [b])
zipBy f g xs ys = zipBy' f g (sortWith f xs) (sortWith g ys)

zipBy' :: Ord c => (a -> c) -> (b -> c) -> [a] -> [b] -> ([(a, b)], [a], [b])
zipBy' _ _  []     ys    = ([], [], ys)
zipBy' _ _  xs     []    = ([], xs, [])
zipBy' f g (x:xs) (y:ys) = case compare (f x) (g y) of
  EQ -> let (xys, xs', ys') = zipBy' f g xs ys in ((x,y):xys, xs', ys')
  LT -> let (xys, xs', ys') = zipBy' f g xs (y:ys) in (xys, x:xs', ys')
  GT -> let (xys, xs', ys') = zipBy' f g (x:xs) ys in (xys, xs', y:ys')

zip3By :: Ord d => (a -> d) -> (b -> d) -> (c -> d) -> [a] -> [b] -> [c] -> ([(a, b, c)], [(a, b)], [(a, c)], [(b, c)], [a], [b], [c])
zip3By f g h xs ys zs = zip3By' f g h (sortWith f xs) (sortWith g ys) (sortWith h zs)

zip3By' :: Ord d => (a -> d) -> (b -> d) -> (c -> d) -> [a] -> [b] -> [c] -> ([(a, b, c)], [(a, b)], [(a, c)], [(b, c)], [a], [b], [c])
zip3By' _ g h  []     ys     zs    = let (yzs, ys', zs') = zipBy' g h ys zs in ([], [], [], yzs, [], ys', zs')
zip3By' f _ h  xs     []     zs    = let (xzs, xs', zs') = zipBy' f h xs zs in ([], [], xzs, [], xs', [], zs')
zip3By' f g _  xs     ys     []    = let (xys, xs', ys') = zipBy' f g xs ys in ([], xys, [], [], xs', ys', [])
zip3By' f g h (x:xs) (y:ys) (z:zs) = let (x', y', z') = (f x, g y, h z) in case (compare x' y', compare x' z', compare y' z') of
  (EQ, EQ, EQ) -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h xs ys zs in ((x,y,z):xyzs, xys, xzs, yzs, xs', ys', zs')

