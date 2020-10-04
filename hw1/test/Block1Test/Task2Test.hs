module Block1Test.Task2Test (test) where

import Block1.Task2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
    testSpec "Natural Numbers" $ do
        describe "toInteger" $ do
            it "toInteger Z"                 $ toInteger Z `shouldBe` 0
            it "toInteger (S Z)"             $ toInteger (S Z) `shouldBe` 1
            it "toIntrger (S (S (S (S Z))))" $ toInteger (S (S (S (S Z)))) `shouldBe` 4
            it "inversion"     $ (fromInteger (toInteger (S (S (S Z)))) :: Nat) `shouldBe` (S (S (S Z)))

        describe "from Integer" $ do
            it "fromInteger 0" $ fromInteger 0 `shouldBe` Z
            it "fromInteger 1" $ fromInteger 1 `shouldBe` S Z
            it "fromInteger 4" $ fromInteger 4 `shouldBe` S (S (S (S Z)))
            it "inversion"     $ toInteger (fromInteger 100 :: Nat) `shouldBe` 100

        describe "addition" $ do
            it "2 + 2" $ 
                (fromInteger 2 :: Nat) + (fromInteger 2 :: Nat) 
                `shouldBe` 
                (fromInteger 4 :: Nat)
            it "0 + 0" $ Z + Z `shouldBe` Z
            it "1 + 100" $ 
                (fromInteger 1 :: Nat) + (fromInteger 100 :: Nat) 
                `shouldBe` 
                (fromInteger 101 :: Nat)
            it "commutativity" $ 
                (fromInteger 4 :: Nat) + (fromInteger 6 :: Nat) 
                `shouldBe` 
                (fromInteger 6 :: Nat) + (fromInteger 4 :: Nat)
            it "associativity" $ 
                ((fromInteger 1 :: Nat) + (fromInteger 2 :: Nat)) + (fromInteger 3 :: Nat) 
                `shouldBe`
                (fromInteger 1 :: Nat) + ((fromInteger 2 :: Nat) + (fromInteger 3 :: Nat)) 

        describe "multiplication" $ do
            it "2 * 2" $
                (fromInteger 2 :: Nat) * (fromInteger 2 :: Nat)
                `shouldBe`
                (fromInteger 4 :: Nat)
            it "1 * 100" $
                (fromInteger 1 :: Nat) * (fromInteger 100 :: Nat)
                `shouldBe`
                (fromInteger 100 :: Nat)
            it "0 * 100" $
                (fromInteger 0 :: Nat) * (fromInteger 100 :: Nat)
                `shouldBe`
                (fromInteger 0 :: Nat)
            it "commutativity" $
                (fromInteger 4 :: Nat) * (fromInteger 6 :: Nat)
                `shouldBe`
                (fromInteger 6 :: Nat) * (fromInteger 4 :: Nat)
            it "associativity" $
                ((fromInteger 2 :: Nat) * (fromInteger 3 :: Nat)) * (fromInteger 4 :: Nat) 
                `shouldBe`
                (fromInteger 2 :: Nat) * ((fromInteger 3 :: Nat) * (fromInteger 4 :: Nat))
            it "distributivity" $
                (fromInteger 2 :: Nat) * ((fromInteger 3 :: Nat) + (fromInteger 4 :: Nat))
                `shouldBe`
                (fromInteger 2 :: Nat) * (fromInteger 3 :: Nat) + (fromInteger 2 :: Nat) * (fromInteger 4 :: Nat)

        describe "subtraction" $ do 
            it "4 - 2" $ 
                (fromInteger 4 :: Nat) - (fromInteger 2 :: Nat) 
                `shouldBe` 
                (fromInteger 2 :: Nat)
            it "0 - 0" $ Z - Z `shouldBe` Z
            it "100 - 0" $ 
                (fromInteger 100 :: Nat) - (fromInteger 0 :: Nat) 
                `shouldBe` 
                (fromInteger 100 :: Nat)
            it "100 - 10" $ 
                (fromInteger 100 :: Nat) - (fromInteger 10 :: Nat) 
                `shouldBe` 
                (fromInteger 90 :: Nat)
        
        describe "equality" $ do 
            it "2 = 2" $
                (fromInteger 2 :: Nat) == (fromInteger 2 :: Nat)
                `shouldBe`
                True
            it "0 = 0" $
                (fromInteger 0 :: Nat) == (fromInteger 0 :: Nat)
                `shouldBe`
                True
            it "4 != 5" $
                (fromInteger 4 :: Nat) /= (fromInteger 5 :: Nat)
                `shouldBe`
                True
            it "!(4 == 5)" $
                (fromInteger 4 :: Nat) == (fromInteger 5 :: Nat)
                `shouldBe`
                False
            it "oppositivity" $
                ((fromInteger 2 :: Nat) == (fromInteger 3 :: Nat)) /= ((fromInteger 2 :: Nat) /= (fromInteger 3 :: Nat))
                `shouldBe`
                True

        describe "comparison" $ do
            it "2 < 3" $
                (fromInteger 2 :: Nat) < (fromInteger 3 :: Nat)
                `shouldBe`
                True
            it "0 < 1" $
                (fromInteger 0 :: Nat) < (fromInteger 1 :: Nat)
                `shouldBe`
                True
            it "100 > 1" $
                (fromInteger 100 :: Nat) > (fromInteger 1 :: Nat)
                `shouldBe`
                True
            it "oppositivity" $
                ((fromInteger 2 :: Nat) < (fromInteger 3 :: Nat)) /= ((fromInteger 2 :: Nat) >= (fromInteger 3 :: Nat))
                `shouldBe`
                True

        describe "evenness" $ do
            it "isEven 0" $
                isEven (fromInteger 0 :: Nat)
                `shouldBe`
                True
            it "isEven 1" $
                isEven (fromInteger 1 :: Nat)
                `shouldBe`
                False
            it "isEven 4" $
                isEven (fromInteger 4 :: Nat)
                `shouldBe`
                True
            it "isEven 5" $
                isEven (fromInteger 5 :: Nat)
                `shouldBe`
                False
        
        describe "division" $ do
            it "div 4 2" $
                (div (fromInteger 4 :: Nat) (fromInteger 2 :: Nat))
                `shouldBe`
                (fromInteger 2 :: Nat)
            it "div 7 2" $
                (div (fromInteger 7 :: Nat) (fromInteger 2 :: Nat))
                `shouldBe`
                (fromInteger 3 :: Nat)
            it "div 3 10" $
                (div (fromInteger 3 :: Nat) (fromInteger 10 :: Nat))
                `shouldBe`
                (fromInteger 0 :: Nat)
            it "div 99 10" $
                (div (fromInteger 99 :: Nat) (fromInteger 10 :: Nat))
                `shouldBe`
                (fromInteger 9 :: Nat)
                
        describe "modulo" $ do
            it "mod 4 2" $
                (mod (fromInteger 4 :: Nat) (fromInteger 2 :: Nat))
                `shouldBe`
                (fromInteger 0 :: Nat)
            it "mod 7 2" $
                (mod (fromInteger 7 :: Nat) (fromInteger 2 :: Nat))
                `shouldBe`
                (fromInteger 1 :: Nat)
            it "mod 3 10" $
                (mod (fromInteger 3 :: Nat) (fromInteger 10 :: Nat))
                `shouldBe`
                (fromInteger 3 :: Nat)
            it "mod 99 10" $
                (mod (fromInteger 99 :: Nat) (fromInteger 10 :: Nat))
                `shouldBe`
                (fromInteger 9 :: Nat)