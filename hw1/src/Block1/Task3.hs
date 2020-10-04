{-# LANGUAGE InstanceSigs #-}

module Block1.Task3
       ( 
           Tree(..),
           isEmpty,
           insert,
           size,
           find,
           fromList,
           toList,
           remove
       ) where

import Data.List.NonEmpty( NonEmpty( (:|) ), (<|) )

data Tree a = Leaf | Node { value :: NonEmpty a
                          , left  :: Tree a
                          , right :: Tree a
                          } deriving(Show)

isEmpty :: Tree a -> Bool

isEmpty Leaf = True
isEmpty _ = False

insert :: Ord a => Tree a -> a -> Tree a

insert Leaf x = Node (x :| []) Leaf Leaf
insert node@(Node vs@(v :| _) l r) x
    | v > x     = node{ left = insert l x }
    | v < x     = node{ right = insert r x }
    | otherwise = node{ value = x <| vs }


size :: Tree a -> Int

size Leaf = 0
size (Node (_ :| xs) l r) = 1 + (length xs) + (size l) + (size r)

find :: Ord a => Tree a -> a -> Bool

find Leaf _ = False
find (Node (v :| _) l r) x 
    | v > x     = find l x
    | v < x     = find r x
    | otherwise = True

fromList :: Ord a => [a] -> Tree a

fromList [] = Leaf
fromList (v:vs) = insert (fromList vs) v

toList :: Tree a -> [a]

toList Leaf = []
toList (Node (v :| rst) l r) = (toList l) ++ (v : rst) ++ (toList r)

concat' :: Tree a -> Tree a -> Tree a

concat' Leaf x = x
concat' node@(Node _ _ r) x = node { right = concat' r x }

remove :: Ord a => Tree a -> a -> Tree a

remove Leaf _ = Leaf
remove node@(Node (v :| rst) l r) x
    | x < v               = node { left = remove l x }
    | x > v               = node { right = remove r x }
    | x == v && rst == [] = concat' l r
    | otherwise           = node { value = (head rst) :| (tail rst) }

-- Block2.Task1

instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b

    foldr _ s Leaf = s
    foldr f s (Node v l r) = lRes where
        rRes = foldr f s r
        mRes = foldr f rRes v 
        lRes = foldr f mRes l

    foldMap :: Monoid m => (a -> m) -> Tree a -> m

    foldMap _ Leaf = mempty
    foldMap f (Node (v :| vs) l r) = lRes <> mRes <> rRes where
        lRes = foldMap f l
        mRes = mconcat (map f (v : vs))
        rRes = foldMap f r