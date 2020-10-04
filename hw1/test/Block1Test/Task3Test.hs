module Block1Test.Task3Test (test) where

import Block1.Task3
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
import Data.List.NonEmpty( NonEmpty( (:|) ))

test :: IO TestTree

test =
    testSpec "Binary Search Tree" $ do
        describe "fromList" $ do
            it "fromList []" $
                toList (fromList [] :: Tree Int)
                `shouldBe`
                []

            it "fromList [1]" $
                toList (fromList [1] :: Tree Int)
                `shouldBe`
                [1]
            it "fromList [1, 2, 2, 4, 5, 5]" $
                toList (fromList [1, 2, 2, 4, 5, 5] :: Tree Int)
                `shouldBe`
                [1, 2, 2, 4, 5, 5]
        
        describe "isEmpty" $ do
            it "isEmpty Leaf" $
                isEmpty Leaf
                `shouldBe`
                True
            it "isEmpty Node" $
                isEmpty (Node ((1 :: Int) :| []) Leaf Leaf)
                `shouldBe`
                False
        
        describe "find" $ do
            it "find in Leaf" $
                find Leaf (1 :: Int)
                `shouldBe`
                False
            it "find if none" $
                find (fromList [1, 2, 2, 4, 5, 5] :: Tree Int) 3
                `shouldBe`
                False
            it "find if single" $
                find (fromList [1, 2, 2, 4, 5, 5] :: Tree Int) 4
                `shouldBe`
                True
            it "find if many" $
                find (fromList [1, 2, 2, 4, 5, 5] :: Tree Int) 5
                `shouldBe`
                True

        describe "size" $ do
            it "size Leaf" $
                size Leaf
                `shouldBe`
                0
            it "size single Node" $
                size (Node ((1 :: Int) :| [1, 1]) Leaf Leaf)
                `shouldBe`
                3
            it "size Tree" $
                size (fromList [1, 2, 2, 4, 5, 5] :: Tree Int)
                `shouldBe`
                6
        
        describe "insert" $ do
            it "insert to Leaf" $
                toList (insert Leaf (1 :: Int))
                `shouldBe`
                [1]
            it "insert if none" $
                toList (insert (fromList [1, 2, 2, 4, 5, 5]) (2 :: Int))
                `shouldBe`
                [1, 2, 2, 2, 4, 5, 5]
            it "insert if some" $
                toList (insert (fromList [1, 2, 2, 4, 5, 5]) (3 :: Int))
                `shouldBe`
                [1, 2, 2, 3, 4, 5, 5]

        describe "remove" $ do
            it "remove if none" $
                toList (remove (fromList [1, 2, 2, 4, 5, 5]) (3 :: Int))
                `shouldBe`
                [1, 2, 2, 4, 5, 5]
            it "remove if single" $
                toList (remove (fromList [1, 2, 2, 4, 5, 5]) (4 :: Int))
                `shouldBe`
                [1, 2, 2, 5, 5]
            it "remove if many" $
                toList (remove (fromList [1, 2, 2, 4, 5, 5]) (5 :: Int))
                `shouldBe`
                [1, 2, 2, 4, 5]