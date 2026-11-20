module BinarySearch (find) where

import Data.Array

arraySlice :: Array Int a -> (Int, Int) -> Array Int a
arraySlice a b
  | fst b < fst (bounds a) || snd b > snd (bounds a) = error "Slice out of bounds"
  | otherwise = array b [(i, a ! i) | i <- [fst b..snd b]]

midpoint :: Array Int a -> (Int, a)
midpoint a = (mid, a ! mid)
  where
    mid = (snd b + fst b) `div` 2
    b = bounds a

find :: Ord a => Array Int a -> a -> Maybe Int
find a e
  | snd (bounds a) < fst (bounds a) = Nothing
  | snd mid == e = Just $ fst mid
  | snd mid < e = find (arraySlice a (fst mid + 1, snd $ bounds a)) e
  | snd mid > e = find (arraySlice a (fst $ bounds a, fst mid - 1)) e
  | otherwise = Nothing
  where
    mid = midpoint a
