module Block3.Task2
       (ok, eof, satisfy, element, stream) where

import           Block3.Task1 (Parser (..))

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)


eof :: Parser s ()
eof = Parser $ \s -> case s of
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (a -> Bool) -> Parser a a
satisfy pr = Parser $ \s -> test s where
    test [] = Nothing
    test (a : as)
        | pr a      = Just (a, as)
        | otherwise = Nothing

element :: (Eq a) => a -> Parser a a
element a = satisfy (== a)

stream :: (Eq a) => [a] -> Parser a [a]
stream = traverse element