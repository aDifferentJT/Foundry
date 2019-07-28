{-# LANGUAGE LambdaCase #-}

module Utils
  ( Bit(..)
  , zipBy
  , zip3By
  , (****)
  ) where

import Control.Arrow ((***))
import Data.List (sortBy)

import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

data Bit = Zero | One
  deriving Eq

instance Show Bit where
  show Zero = "0"
  show One  = "1"

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

data Ordering3
  = ABC
  | ACB
  | BAC
  | BCA
  | CAB
  | CBA
  | AEQ
  | BEQ
  | CEQ
  | EQA
  | EQB
  | EQC
  | EQ3
  deriving Eq

compare3 :: Ord a => a -> a -> a -> Ordering3
compare3 x y z
  | x <  y && y <  z = ABC
  | x <  z && z <  y = ACB
  | y <  x && x <  z = BAC
  | y <  z && z <  x = BCA
  | z <  x && x <  y = CAB
  | z <  y && y <  x = CBA
  | x <  y && y == z = AEQ
  | y <  x && x == z = BEQ
  | z <  x && x == y = CEQ
  | y == z && z <  x = EQA
  | x == z && z <  y = EQB
  | x == y && y <  z = EQC
  | x == y && y == z = EQ3
  | otherwise        = error "Non-transitive Ord instance"

zip3By' :: Ord d => (a -> d) -> (b -> d) -> (c -> d) -> [a] -> [b] -> [c] -> ([(a, b, c)], [(a, b)], [(a, c)], [(b, c)], [a], [b], [c])
zip3By' _ g h  []     ys     zs    = let (yzs, ys', zs') = zipBy' g h ys zs in ([], [], [], yzs, [], ys', zs')
zip3By' f _ h  xs     []     zs    = let (xzs, xs', zs') = zipBy' f h xs zs in ([], [], xzs, [], xs', [], zs')
zip3By' f g _  xs     ys     []    = let (xys, xs', ys') = zipBy' f g xs ys in ([], xys, [], [], xs', ys', [])
zip3By' f g h (x:xs) (y:ys) (z:zs) = case compare3 (f x) (g y) (h z) of
  ABC -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h xs (y:ys) (z:zs) in (xyzs, xys, xzs, yzs, x:xs', ys', zs')
  ACB -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h xs (y:ys) (z:zs) in (xyzs, xys, xzs, yzs, x:xs', ys', zs')
  AEQ -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h xs (y:ys) (z:zs) in (xyzs, xys, xzs, yzs, x:xs', ys', zs')
  BAC -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h (x:xs) ys (z:zs) in (xyzs, xys, xzs, yzs, xs', y:ys', zs')
  BCA -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h (x:xs) ys (z:zs) in (xyzs, xys, xzs, yzs, xs', y:ys', zs')
  BEQ -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h (x:xs) ys (z:zs) in (xyzs, xys, xzs, yzs, xs', y:ys', zs')
  CAB -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h (x:xs) (y:ys) zs in (xyzs, xys, xzs, yzs, xs', ys', z:zs')
  CBA -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h (x:xs) (y:ys) zs in (xyzs, xys, xzs, yzs, xs', ys', z:zs')
  CEQ -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h (x:xs) (y:ys) zs in (xyzs, xys, xzs, yzs, xs', ys', z:zs')
  EQA -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h xs ys (z:zs) in (xyzs, (x,y):xys, xzs, yzs, xs', ys', zs')
  EQB -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h xs (y:ys) zs in (xyzs, xys, (x,z):xzs, yzs, xs', ys', zs')
  EQC -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h (x:xs) ys zs in (xyzs, xys, xzs, (y,z):yzs, xs', ys', zs')
  EQ3 -> let (xyzs, xys, xzs, yzs, xs', ys', zs') = zip3By' f g h xs ys zs in ((x,y,z):xyzs, xys, xzs, yzs, xs', ys', zs')

(****) :: (a -> b -> c) -> (a' -> b' -> c') -> (a, a') -> (b, b') -> (c, c')
f **** g = uncurry (***) . (f *** g)
