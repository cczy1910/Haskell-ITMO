module Block2Test.Task1Test (test) where

import           Block2.Task1
import           Test.Tasty       (TestTree)
import           Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
    testSpec "Expressions on monads" $ do
        describe "Const" $ do
            it "Zero" $
                eval (Const 0)
                `shouldBe`
                Right 0
            it "One" $
                eval (Const 1)
                `shouldBe`
                Right 1
            it "100" $
                eval (Const 100)
                `shouldBe`
                Right 100
        describe "Add" $ do
            it "1 + 1" $
                eval (Add (Const 1) (Const 1))
                `shouldBe`
                Right 2
            it "2 + 2" $
                eval (Add (Const 2) (Const 2))
                `shouldBe`
                Right 4
            it "100 + 0" $
                eval (Add (Const 100) (Const 0))
                `shouldBe`
                Right 100
        describe "Subtract" $ do
            it "1 - 1" $
                eval (Subt (Const 1) (Const 1))
                `shouldBe`
                Right 0
            it "4 - 2" $
                eval (Subt (Const 4) (Const 2))
                `shouldBe`
                Right 2
            it "100 - 1" $
                eval (Subt (Const 100) (Const 1))
                `shouldBe`
                Right 99
        describe "Multiply" $ do
            it "1 * 1" $
                eval (Mul (Const 1) (Const 1))
                `shouldBe`
                Right 1
            it "100 * 0" $
                eval (Mul (Const 100) (Const 0))
                `shouldBe`
                Right 0
            it "2 * 2" $
                eval (Mul (Const 2) (Const 2))
                `shouldBe`
                Right 4
        describe "Divide" $ do
            it "1 / 1" $
                eval (Div (Const 1) (Const 1))
                `shouldBe`
                Right 1
            it "100 / 1" $
                eval (Div (Const 100) (Const 1))
                `shouldBe`
                Right 100
            it "4 / 2" $
                eval (Div (Const 4) (Const 2))
                `shouldBe`
                Right 2
            it "3 / 2" $
                eval (Div (Const 3) (Const 2))
                `shouldBe`
                Right 1
            it "division by zero" $
                eval (Div (Const 100) (Const 0))
                `shouldBe`
                Left (ArithmeticError "Division by zero")
        describe "Pow" $ do
            it "2 ^ 1" $
                eval (Pow (Const 2) (Const 1))
                `shouldBe`
                Right 2
            it "100 ^ 2" $
                eval (Pow (Const 100) (Const 2))
                `shouldBe`
                Right 10000
            it "2 ^ 10" $
                eval (Pow (Const 2) (Const 10))
                `shouldBe`
                Right 1024
            it "100 ^ 0" $
                eval (Pow (Const 100) (Const 0))
                `shouldBe`
                Right 1
            it "Negative power" $
                eval (Pow (Const 100) (Const (-1)))
                `shouldBe`
                Left (ArithmeticError "Neative power")
        describe "Complex expressions" $ do
            it "2 + 2 * 2" $
                eval (Add (Const 2) (Mul (Const 2) (Const 2)))
                `shouldBe`
                Right 6
            it "100 + 100 - (100 + 100)" $
                eval (Subt (Add (Const 100) (Const 100)) (Add (Const 100) (Const 100)))
                `shouldBe`
                Right 0
            it "8 / 3 * 2" $
                eval (Mul (Div (Const 8) (Const 3)) (Const 2))
                `shouldBe`
                Right 4
            it "2 ^ (2 - 3)" $
                eval (Pow (Const 2) (Subt (Const 2) (Const 3)))
                `shouldBe`
                Left (ArithmeticError "Neative power")
            it "(3 / 0) + (2 / 0)" $
                eval (Add (Div (Const 3) (Const 0)) (Div (Const 2) (Const 0)))
                `shouldBe`
                Left (ArithmeticError "Division by zero")
