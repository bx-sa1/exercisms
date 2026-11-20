module POV (fromPOV, tracePathBetween) where

import Data.Tree
import Data.List
import Data.Foldable

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV target = go []
  where
    go ups (Node x ts)
      | x == target = Just $ Node x (ts ++ chain ups)
      | otherwise = asum [go ((x, before ++ after) : ups) k  | (before,k,after) <- picks ts]
    chain [] = []
    chain ((p, sibs) : ps) = [Node p (sibs ++ chain ps)]
    picks xs = [(take i xs, x, drop (i+1) xs) | (i,x) <- zip [0..] xs]

-- Tree ->
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  pa <- pathTo from tree
  pb <- pathTo to tree
  let common = length $ takeWhile id $ zipWith (==) pa pb
      lca = pa !! (common - 1)
      up = reverse $ drop common pa
      down = drop common pb
  return $ up ++ [lca] ++ down
  where
    pathTo n (Node x ts)
      | x == n = Just [x]
      | otherwise = (x :) <$> asum (map (pathTo n) ts)
