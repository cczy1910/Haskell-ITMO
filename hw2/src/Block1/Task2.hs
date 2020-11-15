{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
        (Tree(..)) where

import Control.Applicative

data Tree a
    = Branch (Tree a) (Tree a)
    | Leaf a

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
    fmap f (Leaf a) = Leaf (f a)

instance Applicative Tree where
    pure :: a -> Tree a
    pure = Leaf

    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    (<*>) (Leaf f) (Leaf a) = Leaf (f a)
    (<*>) leaf@(Leaf _) (Branch l r) = (Branch (leaf <*> l) (leaf <*> r))
    (<*>) (Branch l r) leaf@(Leaf _) = (Branch (l <*> leaf) (r <*> leaf))
    (<*>) (Branch lf rf) (Branch la ra) = (Branch (lf <*> la) (rf <*> ra))

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f (Leaf a) = f a
    foldMap f (Branch l r) = (foldMap f l) <> (foldMap f r)

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b) 
    traverse f (Leaf a) = fmap Leaf (f a)
    traverse f (Branch l r) = liftA2 Branch (traverse f l) (traverse f r)