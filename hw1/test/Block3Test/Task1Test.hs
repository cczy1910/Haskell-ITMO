module Block3Test.Task1Test (test) where

import Block3.Task1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
-- import Data.List.NonEmpty( NonEmpty( (:|) ))

test :: IO TestTree
test =
    testSpec "Maybe Concat" $ do
        describe "maybeConcat" $ do 
            it "maybeConcat lists" $
                maybeConcat [Just [(1 :: Int),2,3], Nothing, Just [4,5]]
                `shouldBe`
                [1,2,3,4,5]
            it "maybeConact nothing" $
                maybeConcat [Nothing :: (Maybe [Int]), Nothing, Nothing]
                `shouldBe`
                []
            it "maybeConcat strings" $
                maybeConcat [Just "one", Nothing, Just "two", Nothing, Just "three"]
                `shouldBe`
                "onetwothree"
        -- describe "eitherConcat" $ do
        --     it "eitherConcat lists" :
        --         eitherConcat ([Right [1, 2, 3], Left [3, 2, 1], Right [4, 5]] :: [Either [Int] [Int]])
        --         `shouldBe`
        --         ([3,2,1], [1,2,3,4,5])

        -- describe "foldr" $ do
        --     it "foldr Sum" $ 
        --         foldr (+) 0 (fromList [1, 2, 2, 4, 5, 5] :: Tree Int)
        --         `shouldBe`
        --         19
        --     it "foldr Mul" $ 
        --         foldr (*) 1 (fromList [1, 2, 2, 4, 5, 5] :: Tree Int)
        --         `shouldBe`
        --         400
        --     it "foldr toList sorts" $
        --         foldr (:) [] (fromList [1, 5, 2, 5, 4, 2] :: Tree Int) 
        --         `shouldBe`
        --         [1, 2, 2, 4, 5, 5]
        
        -- describe "foldMap" $ do
        --     it "foldMap toList sorts" $
        --         foldMap (:[]) (fromList [1, 5, 2, 5, 4, 2] :: Tree Int) 
        --         `shouldBe`
        --         [1, 2, 2, 4, 5, 5]