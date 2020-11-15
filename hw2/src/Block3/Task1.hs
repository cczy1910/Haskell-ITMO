{-# LANGUAGE InstanceSigs #-}

module Block3.Task1
       (Parser(..)) where

import Control.Applicative

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser pa) = Parser $ \s -> do
        (a, b) <- pa s
        return ((f a), b)
        

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser (\s -> Just (a, s))

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    (<*>) (Parser pf) (Parser pa) = Parser $ \s -> do
         (f, t) <- pf s
         (a, r) <- pa t
         return ((f a), r)

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    (>>=) (Parser pa) f = Parser $ \s -> do
        (a, t) <- pa s
        (runParser (f a) t)

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser $ \_ -> Nothing

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    (<|>) (Parser pa) (Parser pb) = Parser $ \s -> do
        (pa s) <|> (pb s)

        

