module Block2Test.Task2Test (test) where

import Block2.Task2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
import Data.List.NonEmpty( NonEmpty( (:|) ))

test :: IO TestTree
test =
    testSpec "Splitting" $ do
        describe "splitOn" $ do
            it "splitOn path" $ 
                splitOn '/' "path/to/file"
                `shouldBe`
                "path" :| ["to", "file"]
            it "splitOn telephone" $ 
                splitOn '-' "8-800-555-35-35"
                `shouldBe`
                "8" :| ["800", "555", "35", "35"]
            it "splitOn zeroes" $
                splitOn (0 :: Int) [1, 0, 2, 0, 0, 3, 4, 5, 0, 6, 0, 7]
                `shouldBe`
                [1] :| [[2], [], [3, 4, 5], [6], [7]]
        
        describe "joinWith" $ do
            it "joinWith path" $
                joinWith '/' ("path" :| ["to", "file"])
                `shouldBe`
                "path/to/file"
            it "joinWith zero" $
                joinWith (0 :: Int) ([1] :| [[2], [], [3, 4, 5], [6], [7]])
                `shouldBe`
                [1, 0, 2, 0, 0, 3, 4, 5, 0, 6, 0, 7]
            