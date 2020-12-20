module Main where

import Control.Monad.Except
import Control.Monad.State.Lazy (runState)
import Data.Map.Strict (fromList)
import Lib
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

main :: IO ()
main = do
  tst <- test
  defaultMain $ testGroup "HW3 Test" [tst]

runFS :: MockFS a -> (Either MFSException a, FileSystem)
runFS fsm = runState (runExceptT fsm) example

test :: IO TestTree
test =
  testSpec "File System Operations" $ do
    describe "cd" $ do
      it "cd still" $
        (runFS (cd "."))
          `shouldBe` (Right (), example {currentDirectory = "/home"})
      it "cd up" $
        (runFS (cd "c"))
          `shouldBe` (Right (), example {currentDirectory = "/home/c"})
    describe "ls" $ do
      it "ls" $
        (runFS (ls "."))
          `shouldBe` (Right ["a", "b", "c"], example)
    describe "dir" $ do
      it "dir" $
        (runFS (dir))
          `shouldBe` (Right ["a", "b", "c"], example)
    
        

example :: FileSystem
example =
  FileSystem
    { currentDirectory = "/home",
      root =
        fromList
          [ ( "",
              MockDir
                { name = "",
                  content =
                    fromList
                      [ ( "home",
                          MockDir
                            { name = "home",
                              content =
                                fromList
                                  [ ( "a",
                                      MockFile
                                        { name = "a",
                                          text = "aaa"
                                        }
                                    ),
                                    ( "b",
                                      MockFile
                                        { name = "b",
                                          text = "bbb"
                                        }
                                    ),
                                    ( "c",
                                      MockDir
                                        { name = "c",
                                          content =
                                            fromList
                                              [ ( "d",
                                                  MockFile
                                                    { name = "d",
                                                      text = "ddd"
                                                    }
                                                )
                                              ]
                                        }
                                    )
                                  ]
                            }
                        )
                      ]
                }
            )
          ]
    }