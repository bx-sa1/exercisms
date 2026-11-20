module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List
import Data.Set (size, fromList)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
  | null preorder = Nothing
  | length preorder /= size (fromList preorder) = Nothing
  | otherwise = makeTree preorder inorder
makeTree :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
makeTree ps is = Branch <$> left <*> pure root <*> right
  where
    root = head ps
    (inLeft, rest) = span (/= root) is
    inRight = tail rest
    (preLeft, preRight) = splitAt (length inLeft) $ tail ps
    left
      | null preLeft && null inLeft = Just Leaf
      | null preLeft || null inLeft = Nothing
      | otherwise = makeTree preLeft inLeft
    right
      | null preRight && null inRight = Just Leaf
      | null preRight || null inRight = Nothing
      | otherwise = makeTree preRight inRight
