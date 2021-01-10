module Task3
  ( newCHT,
    getCHT,
    putCHT,
    sizeCHT,
  )
where

import Control.Concurrent.STM
  ( STM,
    TMVar,
    atomically,
    newTMVar,
    putTMVar,
    readTMVar,
    takeTMVar,
  )
import Data.Hashable (Hashable (hash))
import Data.List (deleteBy, find)
import Data.Vector (Vector, replicateM, (!))

data ConcurrentHashTable k v = ConcurrentHashTable
  { elems :: TMVar (Vector (TMVar [(k, v)])),
    sz :: TMVar Int
  }

newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  table <- replicateM 4 (newTMVar [])
  tableRef <- newTMVar table
  s <- newTMVar 0
  return $ ConcurrentHashTable tableRef s

getCHT :: (Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key cht = atomically $ do
  table <- readTMVar (elems cht)
  let i = mod (hash key) (length table)
  bucket <- readTMVar (table ! i)
  return $ snd <$> find (\(k, _) -> k == key) bucket

putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value cht = atomically $ do
  uTable <- takeTMVar (elems cht)
  size <- readTMVar (sz cht)
  table <-
    if 10 * size > 8 * length uTable
      then rehash uTable
      else return uTable
  putTMVar (elems cht) table
  let pos = table ! mod (hash key) (length table)
  bucket <- takeTMVar pos
  let entry = find (\(k, _) -> k == key) bucket
  case entry of
    Nothing -> do
      putTMVar pos ((key, value) : bucket)
      s <- takeTMVar (sz cht)
      putTMVar (sz cht) (s + 1)
    Just v -> do
      putTMVar pos ((key, value) : deleteBy (\(k1, _) (k2, _) -> k1 == k2) v bucket)

rehash :: (Hashable k, Eq k) => Vector (TMVar [(k, v)]) -> STM (Vector (TMVar [(k, v)]))
rehash table = do
  newTable <- replicateM (2 * length table) (newTMVar [])
  oldElems <- mapM readTMVar table
  mapM_ (mapM_ (insertUnchecked newTable)) oldElems
  return newTable

insertUnchecked :: (Hashable k, Eq k) => Vector (TMVar [(k, v)]) -> (k, v) -> STM ()
insertUnchecked table (key, value) = do
  let pos = table ! mod (hash key) (length table)
  bucket <- takeTMVar pos
  putTMVar pos ((key, value) : bucket)

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT cht = atomically $ do
  readTMVar (sz cht)