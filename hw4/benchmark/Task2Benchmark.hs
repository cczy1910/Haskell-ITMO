module Task2Benchmark
  ( integrateBenchmark,
    naiveIntegrateBenchmark,
    integrateCompBenchmark,
    naiveIntegrateCompBenchmark,
  )
where

import Criterion (Benchmark, bench, nf)
import Task2 (integrate, integrateNaive)

integrateBenchmark :: Benchmark
integrateBenchmark = bench "integrate benchmark" $ nf (integrate (\x -> sin x + cos x + tan x) 1000000) (2, 4)

naiveIntegrateBenchmark :: Benchmark
naiveIntegrateBenchmark = bench "naive integrate benchmark" $ nf (integrateNaive (\x -> sin x + cos x + tan x) 1000000) (2, 4)

integrateCompBenchmark :: Benchmark
integrateCompBenchmark = bench "integrate complex benchmark" $ nf (integrate (\x -> (sin . cos . tan) x ** (cos . tan . sin) x ** (tan . sin . cos) x) 1000000) (-1, 1)

naiveIntegrateCompBenchmark :: Benchmark
naiveIntegrateCompBenchmark = bench "naive integrate complex benchmark" $ nf (integrateNaive (\x -> (sin . cos . tan) x ** (cos . tan . sin) x ** (tan . sin . cos) x) 1000000) (-1, 1)
