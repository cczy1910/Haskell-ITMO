module Task3Benchmark
  ( hashtableBenchmark,
  )
where

import Control.Concurrent.Async (mapConcurrently_)
import Criterion (Benchmark, bench, nfIO)
import Task3
  ( newCHT,
    putCHT,
  )

hashtableBenchmark :: Benchmark
hashtableBenchmark = bench "hashtable benchmark" $
  nfIO $ do
    let nb = 5000 :: Int
    cht <- newCHT
    let keys = [0 .. nb] :: [Int]
    mapConcurrently_ (\x -> putCHT x (0 :: Int) cht) keys