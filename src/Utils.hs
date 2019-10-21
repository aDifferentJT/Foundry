{-# LANGUAGE LambdaCase, NoImplicitPrelude #-}

{-|
Module      : Utils
Description : Random utility functions
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental

A few random utility functions, if there were enough of these I would consider splitting it into different modules and possibly a separate package.
-}
module Utils
  ( Bit(Zero, One)
  , Endianness(Little, Big)
  , bitsToInt
  , intToBits
  , mapLeft
  , mapRight
  , mapHead
  , mapLast
  , mapInitLast
  , groupWith
  , selectLargestBy
  , fst3
  , zipBy
  , zip3By
  , (****)
  , joinTailsToHeads
  ) where

import Control.Arrow ((***))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List (foldl, sortBy, unfoldr)
import qualified Data.Map as Map

import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.Read (readPrec)

import ClassyPrelude

-- | A single bit
data Bit
  = Zero -- ^ A 0 bit
  | One  -- ^ A 1 bit
  deriving (Eq, Ord, Enum)

instance Show Bit where
  show Zero = "0"
  show One  = "1"

instance Read Bit where
  readPrec = ReadPrec.get >>= \case
    '0' -> return Zero
    '1' -> return One
    _   -> ReadPrec.pfail

-- | The different endiannesses
data Endianness
  = Little
  | Big

-- | Turn some bits into a number
bitsToInt :: Endianness -> [Bit] -> Int
bitsToInt e = case e of
  Little -> foldr f 0
  Big    -> foldl (flip f) 0
  where f :: Bit -> Int -> Int
        f b = (fromEnum b .|.) . flip shiftL 1

-- | Turn a number into bits
intToBits :: Endianness -> Int -> [Bit]
intToBits Little = unfoldr f
  where f :: Int -> Maybe (Bit, Int)
        f 0 = Nothing
        f x = Just (toEnum (x .&. 1), shiftR x 1)
intToBits Big    = reverse . intToBits Little

-- | Map the left side of an Either
mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f = either (Left . f) Right

-- | Map the right side of an Either
mapRight :: (b1 -> b2) -> Either a b1 -> Either a b2
mapRight f = either Left (Right . f)

-- | Map only the head of the list
mapHead :: (a -> a) -> [a] -> [a]
mapHead _  []    = []
mapHead f (x:xs) = f x : xs

-- | Map only the last element of the list
mapLast :: (a -> a) -> [a] -> [a]
mapLast _  []    = []
mapLast f  [x]   = [f x]
mapLast f (x:xs) = x : mapLast f xs

-- | Map all the elemnts of this list one way bar the last which is mapped a different way
mapInitLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapInitLast _ _ []     = []
mapInitLast _ g [x]    = [g x]
mapInitLast f g (x:xs) = f x : mapInitLast f g xs

-- | Group the elements by their image under the function
groupWith :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupWith f = Map.toList . groupWith' f
  where groupWith' :: Ord b => (a -> b) -> [a] -> Map.Map b [a]
        groupWith' f' = foldr (\x -> Map.insertWith (++) (f' x) [x]) Map.empty

-- | Get the largest element by some metric and the rest
selectLargestBy :: Ord b => (a -> b) -> [a] -> (Maybe a, [a])
selectLargestBy f = first (fst <$>) . foldr g (Nothing, [])
  where g x (Nothing, ys) = (Just (x, f x), ys)
        g x (Just (y, fy), ys)
          | fx <= fy  = (Just (y, fy), x:ys)
          | otherwise = (Just (x, fx), y:ys)
          where fx = f x

-- | Get the first item from a triple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- | Use the given functions to group elements from two lists
zipBy :: Ord c => (a -> c)             -- ^ The function on xs
               -> (b -> c)             -- ^ The function on ys
               -> [a]                  -- ^ The list of xs
               -> [b]                  -- ^ The list of ys
               -> ([(a, b)], [a], [b]) -- ^ Return a tuple of the list of matching pairs that are: in both xs and ys, only in xs and only in ys
zipBy f g xs ys = zipBy' f g (sortWith f xs) (sortWith g ys)

zipBy' :: Ord c => (a -> c) -> (b -> c) -> [a] -> [b] -> ([(a, b)], [a], [b])
zipBy' _ _  []     ys    = ([], [], ys)
zipBy' _ _  xs     []    = ([], xs, [])
zipBy' f g (x:xs) (y:ys) = case compare (f x) (g y) of
  EQ -> let (xys, xs', ys') = zipBy' f g xs ys in ((x,y):xys, xs', ys')
  LT -> let (xys, xs', ys') = zipBy' f g xs (y:ys) in (xys, x:xs', ys')
  GT -> let (xys, xs', ys') = zipBy' f g (x:xs) ys in (xys, xs', y:ys')

-- | Use the given functions to group elements from three lists
zip3By :: Ord d => (a -> d) -- ^ The function on xs
                -> (b -> d) -- ^ The function on ys
                -> (c -> d) -- ^ The function on zs
                -> [a]      -- ^ The list of xs
                -> [b]      -- ^ The list of ys
                -> [c]      -- ^ The list of zs
                            -- | Return a tuple of the list of matching pairs that are in: xs, ys and zs; xs and ys; xs and zs; ys and zs; xs; ys; and zs
                -> ([(a, b, c)], [(a, b)], [(a, c)], [(b, c)], [a], [b], [c])
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

-- | Combine two binary functions to give a binary function on pairs
(****) :: (a -> b -> c)    -- ^ A function \(f\)
       -> (a' -> b' -> c') -- ^ A function \(g\)
       -> (a, a')          -- ^ Values \((x_1, y_1)\)
       -> (b, b')          -- ^ Values \((x_2, y_2)\)
       -> (c, c')          -- ^ \((f(x_1, x_2), g(x_1, x_2))\)
f **** g = uncurry (***) . (f *** g)

joinTailsToHeads :: Monoid m => m -> [[m]] -> [m]
joinTailsToHeads _ [x]        = x
joinTailsToHeads y (x1:x2:xs) = mapLast ((++ (fromMaybe mempty . headMay $ x2)) . (++ y)) x1 ++ joinTailsToHeads y ((fromMaybe mempty . tailMay $ x2) : xs)

