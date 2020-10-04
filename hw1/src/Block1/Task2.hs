module Block1.Task2
       ( 
            Nat(..),
            isEven
       ) where

data Nat = Z | S Nat deriving(Show)

instance Num Nat where
    (+) a Z = a
    (+) a (S b') = S (a + b')
    
    (*) _ Z = Z
    (*) a (S b') = a + (a * b')

    (-) a Z = a
    (-) (S a') (S b') = a' - b'
    (-) Z _ = error "Negative value"

    abs a = a
    
    signum _ = Z

    fromInteger n
        | n == 0    = Z
        | n > 0     = S (fromInteger (n - 1))  
        | otherwise = error "Negative value"

instance Eq Nat where
    (==) Z Z = True
    (==) (S a') (S b') = a' == b'
    (==) _ _ = False

instance Ord Nat where 
    (<=) Z _ = True
    (<=) (S _) Z = False
    (<=) (S a') (S b') = a' <= b'

instance Real Nat where
    toRational = fromIntegral . toInteger

instance Integral Nat where
    toInteger Z = 0
    toInteger (S a') = 1 + (toInteger a')

    quotRem _ Z = error "Division by zero"
    quotRem a b = if a < b
        then (Z, a)
        else (\(x, y) -> ((S x), y)) (quotRem (a - b) b)
    
    div a b = res where
        (res, _) = quotRem a b

    mod a b = res where
        (_, res) = quotRem a b
        
 

instance Enum Nat where
    toEnum = fromInteger . toInteger
    fromEnum = fromInteger . toInteger

isEven :: Nat -> Bool

isEven Z = True
isEven (S Z) = False
isEven (S (S a'')) = isEven a''