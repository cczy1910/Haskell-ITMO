module Block1.Task1
       (stringSum) where

import           Text.Read

stringSum :: String -> Maybe Int

stringSum string = fmap sum (traverse (\x -> readMaybe x :: Maybe Int) (words string))
