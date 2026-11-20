module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Node a (LinkedList a) | Nil deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Node d _) = d
datum Nil = undefined

fromList :: [a] -> LinkedList a
fromList = fromList'
  where
    fromList' (x:rxs) = Node x (fromList' rxs)
    fromList' [] = Nil

isNil :: LinkedList a -> Bool
isNil (Node _ _) = False
isNil Nil = True

new :: a -> LinkedList a -> LinkedList a
new = Node

next :: LinkedList a -> LinkedList a
next (Node _ n) = n
next Nil = Nil

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList = toList'
  where
    toList' (Node d n) = d : toList' n
    toList' Nil = []
