module Block2Test.Task1Test (test) where

import Block1.Task3
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
-- import Data.List.NonEmpty( NonEmpty( (:|) ))

test :: IO TestTree
test =
    testSpec "Foldable for BST" $ do
        describe "foldr" $ do
            it "foldr Sum" $ 
                foldr (+) 0 (fromList [1, 2, 2, 4, 5, 5] :: Tree Int)
                `shouldBe`
                19
            it "foldr Mul" $ 
                foldr (*) 1 (fromList [1, 2, 2, 4, 5, 5] :: Tree Int)
                `shouldBe`
                400
            it "foldr toList sorts" $
                foldr (:) [] (fromList [1, 5, 2, 5, 4, 2] :: Tree Int) 
                `shouldBe`
                [1, 2, 2, 4, 5, 5]
        
        describe "foldMap" $ do
            it "foldMap toList sorts" $
                foldMap (:[]) (fromList [1, 5, 2, 5, 4, 2] :: Tree Int) 
                `shouldBe`
                [1, 2, 2, 4, 5, 5]