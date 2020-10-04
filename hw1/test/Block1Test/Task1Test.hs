module Block1Test.Task1Test (test) where

import Block1.Task1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
    testSpec "Days of Week" $ do
        describe "nextDay" $ do
            it "nextDay Mon" $ nextDay Mon `shouldBe` Tue
            it "nextDay Sun" $ nextDay Sun `shouldBe` Mon
            it "nextDay Wed" $ nextDay Wed `shouldBe` Thu
        
        describe "afterDays" $ do
            it "afterDays Tue 2"   $ afterDays Tue 2 `shouldBe` Thu
            it "afterDays Sat 3"   $ afterDays Sat 3 `shouldBe` Tue
            it "afterDays Mon 7"   $ afterDays Mon 7 `shouldBe` Mon
            it "afterDays Wed 100" $ afterDays Wed 100 `shouldBe` Fri

        describe "isWeekend" $ do
            it "isWeekend Sat"  $ isWeekend Sat `shouldBe` True
            it "isWeekend Sun"  $ isWeekend Sun `shouldBe` True
            it "isWeekend Mon" $ isWeekend Mon `shouldBe` False
            it "isWeekend Fri" $ isWeekend Fri `shouldBe` False

        describe "daysToParty" $ do
            it "daysToParty Mon" $ daysToParty Mon `shouldBe` 4
            it "daysToParty Fri" $ daysToParty Fri `shouldBe` 0
            it "daysToParty Sat" $ daysToParty Sat `shouldBe` 6            