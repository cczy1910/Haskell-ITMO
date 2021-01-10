module Task2
  ( integrateNaive,
    integrate,
  )
where

import Control.Parallel.Strategies (parMap, rpar)
import System.Random (mkStdGen, randomRs)

integrateNaive :: (Double -> Double) -> Int -> (Double, Double) -> Double
integrateNaive f p (l, r) = sum (f <$> take p (randomRs (l, r) (mkStdGen 58110487))) * (r - l) / fromIntegral p

integrate :: (Double -> Double) -> Int -> (Double, Double) -> Double
integrate f p (l, r) = sum (parMap rpar (integrateNaive f nBatches) batches)
  where
    nBatches = floor (sqrt (fromIntegral p :: Double))
    borders = (\x -> l + (r - l) * fromIntegral x / fromIntegral nBatches) <$> [0 .. nBatches]
    batches = zip (init borders) (tail borders)