module Block3.Task2
        (
            NonEmpty(..),
            ThisOrThat(..),
            Name(..),
            Endo(..)
        ) where

data NonEmpty a = a :| [a] deriving(Show)

instance Semigroup (NonEmpty a) where
    (<>) (aF :| aR) (bF :| bR) = aF :| (aR ++ (bF : bR))

instance (Eq a) => Eq (NonEmpty a) where
    (==) (a :| as) (b :| bs) = (a == b) && (as == bs)

data ThisOrThat a b = This a | That b | Both a b deriving(Show)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
    (<>) (This x) (This y)     = This (x <> y)
    (<>) (This x) (That y)     = Both x y
    (<>) (This x) (Both y1 y2) = Both (x <> y1) y2
    (<>) (That x) (This y)     = Both y x
    (<>) (That x) (That y)     = That (x <> y)
    (<>) (That x) (Both y1 y2) = Both y1 (x <> y2)
    (<>) (Both x1 x2) (This y)     = Both (x1 <> y) x2
    (<>) (Both x1 x2) (That y)     = Both x1 (x2 <> y)
    (<>) (Both x1 x2) (Both y1 y2) = Both (x1 <> y1) (x2 <> y2)

instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
    (==) (This a) (This b) = a == b
    (==) (That a) (That b) = a == b
    (==) (Both a1 a2) (Both b1 b2) = a1 == b1 && a2 == b2
    (==) _ _ = False
    
newtype Name = Name String deriving(Show)

instance Semigroup Name where
    (<>) (Name "") (Name b) = Name b
    (<>) (Name a) (Name "") = Name a
    (<>) (Name a) (Name b)  = Name (a ++ ('.' : b))

instance Monoid Name where
     mempty = Name ""

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    (<>) (Endo e1) (Endo e2) = Endo (e1 . e2)

instance Monoid (Endo a) where
    mempty = Endo id