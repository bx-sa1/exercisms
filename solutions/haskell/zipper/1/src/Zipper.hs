module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where
import Data.Maybe

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

type Context a = [(Direction, BinTree a)]
data Direction = DLeft | DRight deriving (Eq, Show)
type Zipper a = (Context a, BinTree a)

fromTree :: BinTree a -> Zipper a
fromTree tree = ([], tree)

toTree :: Zipper a -> BinTree a
toTree ([], t) = t
toTree z = toTree . fromJust . up $ z

value :: Zipper a -> a
value (_, BT v _ _) = v

left :: Zipper a -> Maybe (Zipper a)
left (c, t@(BT _ (Just l) _)) = Just ((DLeft, t) : c, l)
left _ = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (c, t@(BT _ _ (Just r))) = Just ((DRight, t) : c, r)
right _ = Nothing

up :: Zipper a -> Maybe (Zipper a)
up ((DLeft, BT v _ r):cs, t) = Just (cs, BT v (Just t) r)
up ((DRight, BT v l _):cs, t) = Just (cs, BT v l (Just t))
up _ = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue x (c, BT _ l r) = (c, BT x l r)

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree (c, BT v _ r) = (c, BT v tree r)

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree (c, BT v l _) = (c, BT v l tree)
