module Navi.Data.Sorted
  ( Sorted (Nil, (:<:)),
    fromList,
    toList,
    insSorted,
    leastUpperBound,
    unsafeMkSorted,
  )
where

import Data.List (sort)
import Navi.Prelude

data Sorted a
  = Nil
  | Cons a (Sorted a)
  deriving (Eq, Ord)

instance Show a => Show (Sorted a) where
  show Nil = "Nil"
  show (Cons x xs) = show x <> " :<: " <> show xs

infixr 5 `Cons`

unsafeMkSorted :: [a] -> Sorted a
unsafeMkSorted = foldr Cons Nil

fromList :: Ord a => [a] -> Sorted a
fromList = unsafeMkSorted . sort

toList :: Sorted a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

infixr 5 :<:

pattern (:<:) :: Ord a => a -> Sorted a -> Sorted a
pattern x :<: xs <-
  Cons x xs
  where
    x :<: xs = insSorted x xs

{-# COMPLETE (:<:), Nil #-}

insSorted :: Ord a => a -> Sorted a -> Sorted a
insSorted y Nil = Cons y Nil
insSorted y (Cons x xs)
  | y <= x = y `Cons` x `Cons` xs
  | otherwise = x `Cons` insSorted y xs

leastUpperBound :: Ord a => a -> Sorted a -> Maybe a
leastUpperBound _ Nil = Nothing
leastUpperBound y (x :<: xs)
  | y < x = Just x
  | otherwise = leastUpperBound y xs
