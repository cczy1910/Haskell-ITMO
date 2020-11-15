module Main where

import qualified Block2Test.Task1Test (test)
import qualified Block3Test.Task2Test (test)
import qualified Block3Test.Task3Test (test)
import           Test.Tasty           (defaultMain, testGroup)

main :: IO ()
main = do
    testBlock2Task1 <- Block2Test.Task1Test.test
    testBlock3Task2 <- Block3Test.Task2Test.test
    testBlock3Task3 <- Block3Test.Task3Test.test

    defaultMain $ testGroup "HW2 Test" [
        testGroup "Block2" [testBlock2Task1],
        testGroup "Block3" [testBlock3Task2, testBlock3Task3]
        ]
