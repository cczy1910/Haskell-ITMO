{-# LANGUAGE StrictData #-}

module Task1
  ( Point (..),
    Shape,
    plus,
    minus,
    scalarProduct,
    crossProduct,
    perimeter,
    doubleArea,
  )
where

import Data.Foldable (Foldable (foldl'))

data Point = Point Int Int deriving (Show)

type Shape = [Point]

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

len :: Point -> Point -> Double
len a b = let seg = minus a b in sqrt (fromIntegral (scalarProduct seg seg))

foldShape :: Num a => (Point -> Point -> a) -> Shape -> a
foldShape f s = fst $ foldl' foldShapeHepler (0, last s) s
  where
    foldShapeHepler (prevValue, prevPoint) nextPoint = (prevValue + f prevPoint nextPoint, nextPoint)

perimeter :: Shape -> Double
perimeter = foldShape len

doubleArea :: Shape -> Int
doubleArea = foldShape crossProduct