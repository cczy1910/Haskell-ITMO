module Main where

import Criterion.Main (bgroup, defaultMain)
import Task1Benchmark
  ( doubleAreaBenchmark,
    naiveDoubleAreaBenchmark,
    naivePerimeterBenchmark,
    perimeterBenchmark,
  )
import Task2Benchmark
  ( integrateBenchmark,
    integrateCompBenchmark,
    naiveIntegrateBenchmark,
    naiveIntegrateCompBenchmark,
  )
import Task3Benchmark
  ( hashtableBenchmark,
  )

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Task1 benchmark"
        [ perimeterBenchmark,
          naivePerimeterBenchmark,
          doubleAreaBenchmark,
          naiveDoubleAreaBenchmark
        ],
      bgroup
        "Task2 benchmark"
        [ integrateBenchmark,
          naiveIntegrateBenchmark,
          integrateCompBenchmark,
          naiveIntegrateCompBenchmark
        ],
      bgroup
        "Task3 benchmark"
        [ hashtableBenchmark
        ]
    ]