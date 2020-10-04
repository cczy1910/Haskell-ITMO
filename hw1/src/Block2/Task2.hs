module Block2.Task2
        (
            splitOn,
            joinWith       
        ) where

import Data.List.NonEmpty( NonEmpty( (:|) ) )

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]

splitOn e l = foldr f ([]:|[]) l where
    f x (frst :| rst) = 
        if x == e 
            then [] :| (frst : rst)
            else (x : frst) :| rst

joinWith :: a -> NonEmpty [a] -> [a]

joinWith e l = foldl f [] l where
    f [] x = x
    f lst x = lst ++ (e : x)