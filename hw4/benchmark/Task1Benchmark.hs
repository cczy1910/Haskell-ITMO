module Task1Benchmark
  ( perimeterBenchmark,
    naivePerimeterBenchmark,
    doubleAreaBenchmark,
    naiveDoubleAreaBenchmark,
  )
where

import Criterion
import Task1 (Point (..), Shape, doubleArea, perimeter)
import Task1Naive (Point (..), Shape, doubleArea, perimeter)

shape :: Task1.Shape
shape = map (\a -> Task1.Point a (10000001 - a)) [1 .. 10000000]

naiveShape :: Task1Naive.Shape
naiveShape = map (\a -> Task1Naive.Point a (10000001 - a)) [1 .. 10000000]

perimeterBenchmark :: Benchmark
perimeterBenchmark = bench "perimeter benchmark on 1e7 points" $ nf Task1.perimeter shape

naivePerimeterBenchmark :: Benchmark
naivePerimeterBenchmark = bench "naive perimeter benchmark on 1e7 points" $ nf Task1Naive.perimeter naiveShape

doubleAreaBenchmark :: Benchmark
doubleAreaBenchmark = bench "double area benchmark with 1e7 points" $ nf Task1.doubleArea shape

naiveDoubleAreaBenchmark :: Benchmark
naiveDoubleAreaBenchmark = bench "naive double area benchmark with 1e7 points" $ nf Task1Naive.doubleArea naiveShape
