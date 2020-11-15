{-# LANGUAGE InstanceSigs #-}

module Block1.Task3
        (NonEmpty(..)) where

import Control.Applicative

data NonEmpty a = a :| [a] deriving(Show)

instance Semigroup (NonEmpty a) where
    (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y : ys))

instance Functor NonEmpty where
    fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
    fmap f (a :| as) = (f a) :| (fmap f as) 

instance Applicative NonEmpty where
    pure :: a -> NonEmpty a
    pure = (:| [])

    (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
    (<*>) (f :| fs) (a :| as) = fromList ((f : fs) <*> (a : as)) where
        fromList (x : xs) = x :| xs

instance Foldable NonEmpty where
    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (a :| as) = (f a) <> (foldMap f as)

instance Traversable NonEmpty where
    traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b) 
    traverse f (a :| as) = liftA2 (:|) (f a) (traverse f as)

instance Monad NonEmpty where
    (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
    (>>=) a f = foldr1 (<>) (fmap f a)
