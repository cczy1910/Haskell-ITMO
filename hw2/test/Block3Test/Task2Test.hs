module Block3Test.Task2Test (test) where

import           Block3.Task1
import           Block3.Task2
import           Data.Char
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
-- import Data.List.NonEmpty( NonEmpty( (:|) ))

test :: IO TestTree
test =
    testSpec "Basic combinators" $ do
        describe "Ok combinator" $ do
            it "Empty list" $
                (runParser ok) ""
                `shouldBe`
                Just ((), [])
            it "Not empty list" $
                (runParser ok) "abab"
                `shouldBe`
                Just ((), "abab")
        describe "Eof combinator" $ do
            it "Empty list" $
                (runParser eof) ""
                `shouldBe`
                Just ((), [])
            it "Not empty list" $
                (runParser eof) "abab"
                `shouldBe`
                Nothing
        describe "Satisfy combinator" $ do
            it "Anything" $
                (runParser (satisfy (const True))) "abab"
                `shouldBe`
                Just ('a', "bab")
            it "Equality" $
                (runParser (satisfy (== 'a'))) "abab"
                `shouldBe`
                Just ('a', "bab")
            it "Digit" $
                (runParser (satisfy isDigit)) "1.1"
                `shouldBe`
                Just ('1', ".1")
            it "Not a digit" $
                (runParser (satisfy isDigit)) "abab"
                `shouldBe`
                Nothing
            it "Numeric predicate true" $
                (runParser (satisfy (> (2 :: Int)))) [3, 4, 5]
                `shouldBe`
                Just (3, [4, 5])
            it "Numeric predicate false" $
                (runParser (satisfy (== (2 :: Int)))) [3, 4, 5]
                `shouldBe`
                Nothing
        describe "Element combinator" $ do
            it "No element" $
                (runParser (element 'a')) ""
                `shouldBe`
                Nothing
            it "Have element" $
                (runParser (element 'a')) "abab"
                `shouldBe`
                Just ('a', "bab")
            it "Wrong element" $
                (runParser (element 'b')) "abab"
                `shouldBe`
                Nothing
        describe "Stream combinator" $ do
            it "Empty and empty" $
                (runParser (stream "")) ""
                `shouldBe`
                Just ("", "")
            it "Empty and not empty" $
                (runParser (stream "")) "abab"
                `shouldBe`
                Just ("", "abab")
            it "Not empty and empty" $
                (runParser (stream "abab")) ""
                `shouldBe`
                Nothing 
            it "Match" $
                (runParser (stream "abab")) "abab"
                `shouldBe`
                Just ("abab", "")
            it "Prefix" $
                (runParser (stream "ab")) "abab"
                `shouldBe`
                Just ("ab", "ab")
            it "Incorrect" $
                (runParser (stream "ba")) "abab"
                `shouldBe`
                Nothing
            it "Matching prfix" $
                (runParser (stream "aa")) "abab"
                `shouldBe`
                Nothing