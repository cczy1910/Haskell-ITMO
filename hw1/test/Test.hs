module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Block1Test.Task1Test (test)
import qualified Block1Test.Task2Test (test)
import qualified Block1Test.Task3Test (test)
import qualified Block2Test.Task1Test (test)
import qualified Block2Test.Task2Test (test)
import qualified Block3Test.Task1Test (test)
import qualified Block3Test.Task2Test (test)

main :: IO ()
main = do 
    testBlock1Task1 <- Block1Test.Task1Test.test
    testBlock1Task2 <- Block1Test.Task2Test.test
    testBlock1Task3 <- Block1Test.Task3Test.test
    testBlock2Task1 <- Block2Test.Task1Test.test
    testBlock2Task2 <- Block2Test.Task2Test.test
    testBlock3Task1 <- Block3Test.Task1Test.test
    testBlock3Task2 <- Block3Test.Task2Test.test

    defaultMain $ testGroup "HW1 Test" [
        testGroup "Block1" [testBlock1Task1, testBlock1Task2, testBlock1Task3],
        testGroup "Block2" [testBlock2Task1, testBlock2Task2],
        testGroup "Block3" [testBlock3Task1, testBlock3Task2]
        ]