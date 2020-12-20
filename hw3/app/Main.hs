module Main where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.IORef
import Lib
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.IO

data Command
  = Cd FilePath
  | Dir
  | Ls FilePath
  | CreateFolder String
  | Cat FilePath
  | CreateFile String
  | Remove FilePath
  | WriteFile FilePath [String]
  | FindFile String
  | Information FilePath
  | Exit

main :: IO ()
main = do
  curDir <- getCurrentDirectory
  dirRef <- newIORef curDir
  let cmds = runMaybeT $ (asum . repeat) getCommand
  Just _ <- runReaderT cmds dirRef
  return ()

getCommand :: MaybeT RealFS ()
getCommand = do
  curDirRef <- lift ask
  curDir <- lift $ lift $ readIORef curDirRef
  lift $ lift $ putStr (curDir ++ "> ")
  lift $ lift $ hFlush stdout
  cmd <- lift (lift getLine)
  catchError (parseCommand cmd) (\e -> (lift $ lift $ putStrLn $ show e) >> empty)

parseCommand :: String -> MaybeT RealFS ()
parseCommand cmd = parseCommandHelper $ execParserPure defaultPrefs commandParserInfo (words cmd)

parseCommandHelper :: ParserResult Command -> MaybeT RealFS ()
parseCommandHelper (Success res) = runCommand res
parseCommandHelper (Failure (ParserFailure hlp)) = (lift $ lift $ putStrLn $ let (msg, _, _) = hlp "<Programm>" in show msg) >> empty
parseCommandHelper _ = return ()

commandParserInfo :: ParserInfo Command
commandParserInfo = info (commandParser <**> helper) idm

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "ls"
        ( info
            (Ls <$> argument str (metavar "DIRNAME"))
            (progDesc "Show directory content")
        )
        <> command
          "cd"
          ( info
              (Cd <$> argument str (metavar "DIRNAME"))
              (progDesc "Change current directory")
          )
        <> command
          "dir"
          ( info
              (pure Dir)
              (progDesc "Show current directory content")
          )
        <> command
          "exit"
          ( info
              (pure Exit)
              (progDesc "Stop program")
          )
        <> command
          "create-folder"
          ( info
              (CreateFolder <$> argument str (metavar "DIRNAME"))
              (progDesc "Create new folder")
          )
        <> command
          "cat"
          ( info
              (Cat <$> argument str (metavar "FILENAME"))
              (progDesc "Show file content")
          )
        <> command
          "create-file"
          ( info
              (CreateFile <$> argument str (metavar "FILENAME"))
              (progDesc "Create new file")
          )
        <> command
          "find-file"
          ( info
              (FindFile <$> argument str (metavar "FILENAME"))
              (progDesc "Create new file")
          )
        <> command
          "remove"
          ( info
              (Remove <$> argument str (metavar "FILENAME|DIRNAME"))
              (progDesc "Remove file or directory")
          )
        <> command
          "information"
          ( info
              (Information <$> argument str (metavar "FILENAME|DIRNAME"))
              (progDesc "Show information about file or folder")
          )
        <> command
          "write-file"
          ( info
              (WriteFile <$> argument str (metavar "FILENAME") <*> (many (argument str (metavar "CONTENT"))))
              (progDesc "Write text to file")
          )
        <> command
          "information"
          ( info
              (Information <$> argument str (metavar "FILENAME|DIRNAME"))
              (progDesc "Show information about file or folder")
          )
    )

runCommand :: Command -> MaybeT RealFS ()
runCommand Exit = return ()
runCommand c = (lift $ runCommandHelper c) >> empty
  where
    runCommandHelper :: Command -> RealFS ()
    runCommandHelper (Cd path) = cd path
    runCommandHelper Dir = do
      res <- dir
      lift $ sequence_ (putStrLn <$> res)
    runCommandHelper (Ls path) = do
      res <- ls path
      lift $ sequence_ (putStrLn <$> res)
    runCommandHelper (CreateFolder fname) = createFolder fname
    runCommandHelper (Cat path) = do
      res <- cat path
      lift $ putStrLn res
    runCommandHelper (CreateFile fname) = createFile fname
    runCommandHelper (Remove path) = remove path
    runCommandHelper (WriteFile path val) = writeToFile path (unwords val)
    runCommandHelper (FindFile fname) = do
      res <- findFile fname
      lift $ sequence_ (putStrLn <$> res)
    runCommandHelper (Information path) = do
      res <- information path
      lift $ putStrLn res
    runCommandHelper Exit = return ()

runCd :: FilePath -> RealFS ()
runCd path = do
  cd path

runLs :: FilePath -> RealFS ()
runLs path = do
  res <- ls path
  lift $ sequence_ (putStrLn <$> res)