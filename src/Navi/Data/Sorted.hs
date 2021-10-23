-- | Provides the 'Sorted' type for ensuring data is sorted.
module Navi.Data.Sorted
  ( Sorted (Nil, (:<:)),
    fromList,
    toList,
    insSorted,
    leastUpperBound,
  )
where

import Data.List (sort)
import Navi.Prelude

-- | Represents a sorted list.
data Sorted a
  = Nil
  | Cons a (Sorted a)
  deriving (Eq, Generic, Ord)

instance Show a => Show (Sorted a) where
  show Nil = "Nil"
  show (Cons x xs) = show x <> " :<: " <> show xs

infixr 5 `Cons`

-- | Unsafely constructs a 'Sorted' from a list. This is intended to be used
-- when a list is already sorted, for performance. Runs in
-- \(\mathcal{O}(n)\). Exercise restraint!
unsafeMkSorted :: [a] -> Sorted a
unsafeMkSorted = foldr Cons Nil

-- | Sorts the input list and returns a 'Sorted'. Runs in \(\mathcal{O}(n\log{}n)\).
--
-- >>> fromList [6,2,5]
-- 2 :<: 5 :<: 6 :<: Nil
fromList :: Ord a => [a] -> Sorted a
fromList = unsafeMkSorted . sort

-- | Returns a regular list. Runs in \(\mathcal{O}(n)\).
--
-- >>> toList $ 2 :<: 5 :<: 8 :<: Nil
-- [2,5,8]
toList :: Sorted a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

infixr 5 :<:

-- | Constructor for 'Sorted'. Allows pattern matching and inserting an
-- element in sorted order. Runs in \(\mathcal{O}(n)\).
--
-- >>> 7 :<: 2 :<: Nil
-- 2 :<: 7 :<: Nil
pattern (:<:) :: Ord a => a -> Sorted a -> Sorted a
pattern x :<: xs <-
  Cons x xs
  where
    x :<: xs = insSorted x xs

{-# COMPLETE (:<:), Nil #-}

-- | Inserts an element into sorted order. Runs in \(\mathcal{O}(n)\)
--
-- >>> insSorted 7 $ 2 :<: 10 :<: Nil
-- 2 :<: 7 :<: 10 :<: Nil
insSorted :: Ord a => a -> Sorted a -> Sorted a
insSorted y Nil = Cons y Nil
insSorted y (Cons x xs)
  | y <= x = y `Cons` x `Cons` xs
  | otherwise = x `Cons` insSorted y xs

-- | Given \(y, xs\), returns the first \(x \in xs\) with \(y < x\). Runs in
-- \(\mathcal{O}(n)\).
--
-- >>> leastUpperBound 12 $ 2 :<: 7 :<: 10 :<: Nil
-- Nothing
--
-- >>> leastUpperBound 5 $ 2 :<: 7 :<: 10:<: Nil
-- Just 7
leastUpperBound :: Ord a => a -> Sorted a -> Maybe a
leastUpperBound _ Nil = Nothing
leastUpperBound y (x :<: xs)
  | y < x = Just x
  | otherwise = leastUpperBound y xs
