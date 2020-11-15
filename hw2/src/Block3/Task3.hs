module Block3.Task3
       (parseCbs, parseInt) where

import Block3.Task1
import Block3.Task2

import Control.Applicative
import Data.Char


(<<>>) :: Semigroup a => Parser s a -> Parser s a -> Parser s a 
(<<>>) (Parser pa) (Parser pb) = Parser $ \s -> do
    (a, t) <- pa s
    (b, r) <- pb t
    return ((a <> b), r)

emptyOk :: Monoid a => Parser s a
emptyOk = Parser $ \s -> Just (mempty, s)

parseCbs :: Parser Char [Char]
parseCbs = cbs <* eof where
    cbs = (stream "(" <<>> cbs <<>> stream ")" <<>> cbs) <|> emptyOk

parseInt :: Parser Char Int
parseInt = fmap read (uns <|> pos <|> neg) where
    uns = some (satisfy isDigit)
    pos = (element '+') *> uns
    neg = (stream "-") <<>> uns
