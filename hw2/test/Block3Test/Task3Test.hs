module Block3Test.Task3Test (test) where

import           Block3.Task1
import           Block3.Task3
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
-- import Data.List.NonEmpty( NonEmpty( (:|) ))

test :: IO TestTree
test =
    testSpec "Simple parsers" $ do
        describe "Parse CBS" $ do
            it "Empty" $
                (runParser parseCbs) ""
                `shouldBe`
                Just ("", "")
            it "Not a bracket" $
                (runParser parseCbs) "abab"
                `shouldBe`
                Nothing
            it "Correct 1" $
                (runParser parseCbs) "()"
                `shouldBe`
                Just ("()", "")
            it "Correct 2" $
                (runParser parseCbs) "(())"
                `shouldBe`
                Just ("(())", "")
            it "Correct 3" $
                (runParser parseCbs) "()()"
                `shouldBe`
                Just ("()()", "")
            it "Correct 4" $
                (runParser parseCbs) "(())()"
                `shouldBe`
                Just ("(())()", "")
            it "Correct 5" $
                (runParser parseCbs) "(()())(())"
                `shouldBe`
                Just ("(()())(())", "")
            it "Incorrect 1" $
                (runParser parseCbs) "("
                `shouldBe`
                Nothing
            it "Incorrect 2" $
                (runParser parseCbs) ")"
                `shouldBe`
                Nothing
            it "Incorrect 3" $
                (runParser parseCbs) "((())"
                `shouldBe`
                Nothing
            it "Incorrect 4" $
                (runParser parseCbs) "()()())"
                `shouldBe`
                Nothing
            it "Incorrect 5" $
                (runParser parseCbs) ")))))"
                `shouldBe`
                Nothing
            it "Incorrect 6" $
                (runParser parseCbs) "((((("
                `shouldBe`
                Nothing
            it "Incorrect 7" $
                (runParser parseCbs) "(()))(()))"
                `shouldBe`
                Nothing
        describe "Parse Int" $ do
            it "Zero" $
                (runParser parseInt) "0"
                `shouldBe`
                Just (0, "")
            it "100" $
                (runParser parseInt) "100"
                `shouldBe`
                Just (100, "")
            it "Tail" $
                (runParser parseInt) "100abab"
                `shouldBe`
                Just (100, "abab")
            it "Two numbers" $
                (runParser parseInt) "100 200"
                `shouldBe`
                Just (100, " 200")
            it "Negative" $
                (runParser parseInt) "-100"
                `shouldBe`
                Just (-100, "")
            it "Positive" $
                (runParser parseInt) "+100"
                `shouldBe`
                Just (100, "")
            it "Big number" $
                (runParser parseInt) "1000000000"
                `shouldBe`
                Just (1000000000, "")
            it "Space at start" $
                (runParser parseInt) " 100"
                `shouldBe`
                Nothing
            it "Not a number" $
                (runParser parseInt) "NaN"
                `shouldBe`
                Nothing
            it "Expression" $
                (runParser parseInt) "100+200"
                `shouldBe`
                Just (100, "+200")
