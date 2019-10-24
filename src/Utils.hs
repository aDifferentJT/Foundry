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
  , intersectionWithKey3
  , (****)
  , joinTailsToHeads
  , whileM
  , untilM
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
  Little -> foldl (flip f) 0
  Big    -> foldr f 0
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

intersectionWithKey3 :: Ord k => (k -> a -> b -> c -> d) -> Map k a -> Map k b -> Map k c -> Map k d
intersectionWithKey3 f m1 m2 m3 = Map.intersectionWithKey (uncurry . f) (Map.intersectionWith (,) m1 m2) m3

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

whileM :: Monad m => Int -> (a -> Bool) -> m a -> m (Maybe a)
whileM 0 _ _ = return Nothing
whileM n f m = m >>= \x -> if f x then whileM (n-1) f m else return . Just $ x

untilM :: Monad m => Int -> (a -> Bool) -> m a -> m (Maybe a)
untilM 0 _ _ = return Nothing
untilM n f m = m >>= \x -> if f x then return . Just $ x else untilM (n-1) f m

