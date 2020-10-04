module Block3Test.Task2Test (test) where

import Block3.Task2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
-- import Data.List.NonEmpty( NonEmpty( (:|) ))

test :: IO TestTree
test =
    testSpec "Semigroups" $ do
        describe "NonEmpty" $ do
            it "NonEmpty operation" $ 
                ((1 :: Int) :| [2]) <> (3 :| [4])
                `shouldBe`
                1 :| [2, 3, 4]
        describe "ThisOrThat operation" $ do
            it "This <> This" $ 
                ((This [1, 2] :: ThisOrThat [Int] [Int]) <> (This [3, 4] :: ThisOrThat [Int] [Int])) 
                `shouldBe` 
                This [1, 2, 3, 4]
            it "This <> That" $
                ((This [1, 2] :: ThisOrThat [Int] [Int]) <> (That [3, 4] :: ThisOrThat [Int] [Int])) 
                `shouldBe` 
                Both [1, 2] [3, 4]
            it "Both <> Both" $
                ((Both [1] [3] :: ThisOrThat [Int] [Int]) <> (Both [2] [4] :: ThisOrThat [Int] [Int])) 
                `shouldBe` 
                Both [1, 2] [3, 4]