{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lib
  ( FSActions (..),
    RealFS,
    MockFS,
    Navigable (..),
    FileSystem (..),
    FSTree (..),
    MFSException,
  )
where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map.Strict
  ( Map,
    elems,
    empty,
    insert,
    member,
    notMember,
    update,
    (!?),
  )
import System.Directory
import System.FilePath
import System.IO.Error

type RealFS = ReaderT (IORef FilePath) IO

class FSActions m where
  relativize :: FilePath -> m FilePath
  cd :: FilePath -> m ()
  dir :: m [FilePath]
  ls :: FilePath -> m [FilePath]
  createFolder :: String -> m ()
  cat :: FilePath -> m String
  createFile :: String -> m ()
  remove :: FilePath -> m ()
  writeToFile :: FilePath -> String -> m ()
  findFile :: String -> m [FilePath]
  information :: FilePath -> m String

walkDirectory :: (FilePath -> a) -> (FilePath -> [a]) -> FilePath -> RealFS [a]
walkDirectory forDir forFile path = do
  isDir <- lift $ doesDirectoryExist path
  if isDir
    then do
      sub <- ls path
      let subPaths = (path </>) <$> sub
      subRes <- traverse (walkDirectory forDir forFile) subPaths
      return $ (forDir path) : (concat subRes)
    else return $ forFile path

subdirectories :: FilePath -> RealFS [FilePath]
subdirectories = walkDirectory id (const [])

filesCount :: FilePath -> RealFS Integer
filesCount path = do
  isDir <- lift $ doesDirectoryExist path
  if isDir
    then do
      sub <- ls path
      let subPaths = (path </>) <$> sub
      subRes <- sum <$> (traverse filesCount subPaths)
      return $ 1 + subRes
    else return 1

instance FSActions RealFS where
  relativize :: FilePath -> RealFS FilePath
  relativize path = do
    curPathRef <- ask
    curPath <- lift $ readIORef curPathRef
    lift $ canonicalizePath (curPath </> path)

  cd :: FilePath -> RealFS ()
  cd directory = do
    relPath <- relativize directory
    curPathRef <- ask
    exists <- lift $ doesDirectoryExist relPath
    if exists
      then lift $ writeIORef curPathRef relPath
      else lift $ throwIO $ mkIOError doesNotExistErrorType "setCurrentDirectory" Nothing (Just relPath)

  ls :: FilePath -> RealFS [FilePath]
  ls directory = do
    relPath <- (relativize directory)
    lift $ listDirectory relPath

  dir :: RealFS [FilePath]
  dir = ls "."

  createFolder :: String -> RealFS ()
  createFolder fName = do
    relPath <- relativize fName
    lift $ createDirectory relPath

  cat :: FilePath -> RealFS String
  cat file = do
    relPath <- relativize file
    lift $ readFile relPath

  createFile :: String -> RealFS ()
  createFile fName = do
    relPath <- relativize fName
    exists <- lift $ doesPathExist relPath
    if exists
      then lift $ throwIO $ mkIOError alreadyExistsErrorType "createFile" Nothing (Just relPath)
      else lift $ writeFile relPath ""

  remove :: FilePath -> RealFS ()
  remove fileOrFolder = do
    relPath <- relativize fileOrFolder
    isFile <- lift $ doesFileExist relPath
    if isFile
      then lift $ removeFile relPath
      else lift $ removeDirectory relPath

  writeToFile :: FilePath -> String -> RealFS ()
  writeToFile file val = do
    relPath <- relativize file
    lift $ writeFile relPath val

  findFile :: String -> RealFS [FilePath]
  findFile fName = do
    curPathRef <- ask
    curPath <- lift $ readIORef curPathRef
    sub <- subdirectories curPath
    lift $ findFiles sub fName

  information :: FilePath -> RealFS String
  information fileOrDirectory = do
    relPath <- relativize fileOrDirectory
    let fName = takeFileName relPath
    perms <- lift $ getPermissions relPath
    modTime <- lift $ (getModificationTime relPath)
    isDir <- lift $ doesDirectoryExist relPath
    sze <-
      if isDir
        then filesCount relPath
        else lift $ getFileSize relPath
    return $ "name: " ++ fName ++ "\nperms: " ++ (show perms) ++ "\nsize: " ++ (show sze) ++ "\nmodification time: " ++ (show modTime)

data FileSystem = FileSystem
  { currentDirectory :: FilePath,
    root :: Map String FSTree
  }
  deriving (Show, Eq)

data FSTree
  = MockFile
      { name :: String,
        text :: String
      }
  | MockDir
      { name :: String,
        content :: Map String FSTree
      }
  deriving (Show, Eq, Ord)

data MFSException
  = AlreadyExistsError FilePath
  | DoesNotExistError FilePath
  | NotAFileError FilePath
  | NotADirectoryError FilePath
  deriving (Show, Eq)

instance Exception MFSException

type MockFS = ExceptT MFSException (State FileSystem)

class Navigable m where
  mockExists :: FilePath -> m Bool
  mockIsDir :: FilePath -> m Bool
  mockIsFile :: FilePath -> m Bool
  mockGet :: FilePath -> m (Maybe FSTree)
  mockPut :: FSTree -> FilePath -> m ()
  mockMod :: (FSTree -> Maybe (FSTree)) -> FilePath -> m ()
  mockRem :: FilePath -> m ()

instance Navigable MockFS where
  mockGet :: FilePath -> MockFS (Maybe FSTree)
  mockGet path = do
    contents <- lift $ root <$> get
    return $ getHelper (splitOn "/" path) contents
    where
      getHelper :: [String] -> Map String FSTree -> Maybe FSTree
      getHelper [] _ = Nothing
      getHelper (s : []) trees = trees !? s
      getHelper (s : ses) trees = case trees !? s of
        Just (MockDir _ nTrees) -> getHelper ses nTrees
        _ -> Nothing

  mockExists :: FilePath -> MockFS Bool
  mockExists path = do
    res <- mockGet path
    case res of
      Just _ -> return True
      _ -> return False

  mockIsDir :: FilePath -> MockFS Bool
  mockIsDir path = do
    res <- mockGet path
    case res of
      Just (MockDir _ _) -> return True
      _ -> return False

  mockIsFile :: FilePath -> MockFS Bool
  mockIsFile path = do
    res <- mockGet path
    case res of
      Just (MockFile _ _) -> return True
      _ -> return False

  mockMod :: (FSTree -> Maybe (FSTree)) -> FilePath -> MockFS ()
  mockMod f path = do
    contents <- lift $ root <$> get
    case modHelper (splitOn "/" path) contents of
      Just newRoot -> lift $ modify (\x -> x {root = newRoot})
      _ -> throw $ DoesNotExistError path
    where
      modHelper :: [String] -> Map String FSTree -> Maybe (Map String FSTree)
      modHelper [] _ = Nothing
      modHelper (s : []) trees =
        if member s trees
          then Just $ update f s trees
          else Nothing
      modHelper (s : ses) trees = case trees !? s of
        Just d@(MockDir _ nTrees) -> case modHelper ses nTrees of
          Just res -> Just $ insert s (d {content = res}) trees
          _ -> Nothing
        _ -> Nothing

  mockPut :: FSTree -> FilePath -> MockFS ()
  mockPut v path = do
    contents <- lift $ root <$> get
    case putHelper (splitOn "/" path) contents of
      Right newRoot -> lift $ modify (\x -> x {root = newRoot})
      _ -> throw $ DoesNotExistError path
    where
      putHelper :: [String] -> Map String FSTree -> Either MFSException (Map String FSTree)
      putHelper [] _ = Left $ DoesNotExistError path
      putHelper (s : []) trees =
        if notMember s trees
          then Right $ insert s v trees
          else Left $ AlreadyExistsError path
      putHelper (s : ses) trees = case trees !? s of
        Just d@(MockDir _ nTrees) -> case putHelper ses nTrees of
          Right res -> Right $ insert s (d {content = res}) trees
          err -> err
        _ -> Left $ DoesNotExistError path

  mockRem :: FilePath -> MockFS ()
  mockRem = mockMod $ const Nothing

dropSlash :: FilePath -> FilePath
dropSlash path = case reverse path of
  ('/' : rest) -> reverse rest
  _ -> path

instance FSActions MockFS where
  relativize :: FilePath -> MockFS FilePath
  relativize path = do
    curPath <- lift $ currentDirectory <$> get
    return $ intercalate "/" (relativizeHelper [] $ splitOn "/" $ dropSlash (normalise (curPath </> path)))
    where
      relativizeHelper :: [String] -> [String] -> [String]
      relativizeHelper cs [] = reverse cs
      relativizeHelper cs ("." : ts) = relativizeHelper cs ts
      relativizeHelper [""] (".." : ts) = relativizeHelper [""] ts
      relativizeHelper (_ : cs) (".." : ts) = relativizeHelper cs ts
      relativizeHelper cs (t : ts) = relativizeHelper (t : cs) ts

  cd :: FilePath -> MockFS ()
  cd directory = do
    relPath <- relativize directory
    exists <- mockExists relPath
    if exists
      then lift $ modify (\x -> x {currentDirectory = relPath})
      else throw $ DoesNotExistError relPath

  ls :: FilePath -> MockFS [FilePath]
  ls directory = do
    relPath <- relativize directory
    target <- mockGet relPath
    case target of
      Just (MockDir _ val) -> return $ elems $ name <$> val
      Just (MockFile _ _) -> throw $ NotADirectoryError relPath
      _ -> throw $ DoesNotExistError relPath

  dir :: MockFS [FilePath]
  dir = ls "."

  createFolder :: String -> MockFS ()
  createFolder directory = do
    relPath <- relativize directory
    mockPut (MockDir (takeFileName relPath) empty) relPath

  cat :: FilePath -> MockFS String
  cat file = do
    relPath <- relativize file
    target <- mockGet relPath
    case target of
      Just (MockFile _ val) -> return val
      Just (MockDir _ _) -> throw $ NotAFileError relPath
      Nothing -> throw $ DoesNotExistError relPath

  createFile :: String -> MockFS ()
  createFile file = do
    relPath <- relativize file
    mockPut (MockFile (takeFileName relPath) "") relPath

  remove :: FilePath -> MockFS ()
  remove fileOrFolder = do
    relPath <- relativize fileOrFolder
    mockRem relPath

  writeToFile :: FilePath -> String -> MockFS ()
  writeToFile file val = do
    relPath <- relativize file
    target <- mockGet relPath
    case target of
      Just f@(MockFile _ _) -> mockPut (f {text = val}) relPath
      Just (MockDir _ _) -> throw $ NotAFileError relPath
      Nothing -> mockPut (MockFile (takeFileName relPath) val) relPath

  findFile :: String -> MockFS [FilePath]
  findFile = const $ return ["not inplemented :("]

  information :: FilePath -> MockFS String
  information = const $ return "not inplemented :("