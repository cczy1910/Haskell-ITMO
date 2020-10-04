module Block3.Task1
        (
            maybeConcat,
            eitherConcat
        ) where

maybeConcat :: [Maybe [a]] -> [a]

maybeConcat values = f (mconcat values) where
    f Nothing = []
    f (Just res) = res 

eitherConcat :: (Monoid a, Monoid b) => [ Either a b ] -> (a, b)

eitherConcat l = mconcat (map eithetToPair l) where
    eithetToPair (Left a) = (a, mempty)
    eithetToPair (Right b) = (mempty, b)