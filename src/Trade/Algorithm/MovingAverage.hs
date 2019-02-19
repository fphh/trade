

module Trade.Algorithm.MovingAverage where

import Statistics.Sample (mean)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.List (scanl')

newtype WindowSize = WindowSize Int deriving (Show, Eq, Ord)

movingAvgL :: WindowSize -> [Double] -> [Double]
movingAvgL (WindowSize k) lst =
  let (h, t) = splitAt k lst 
  in map (/ fromIntegral k) $ scanl' (+) (sum h) $ zipWith (-) t lst


mavgL :: WindowSize -> [Double] -> [(Double, Double)]
mavgL w@(WindowSize k) xs =
  let j = fromIntegral k -- / 2
  in zip [j, j+1 ..] (movingAvgL w xs)


movingAvgV :: WindowSize -> Vector Double -> Vector Double
movingAvgV (WindowSize k) v =
  let len = Vec.length v
      f a = Vec.slice a k v
      idx = Vec.generate (len-k) id
  in Vec.map (mean . f) idx

mavgV :: WindowSize -> Vector Double -> Vector (Double, Double)
mavgV (WindowSize k) v =
  let len = Vec.length v
      f a = (fromIntegral (k+a), mean (Vec.slice a k v))
  in Vec.generate (len-k) f

mavg3 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg3 = mavg (WindowSize 3)

mavg5 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg5 = mavg (WindowSize 5)

mavg8 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg8 = mavg (WindowSize 8)

mavg12 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg12 = mavg (WindowSize 12)

mavg15 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg15 = mavg (WindowSize 15)

mavg20 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg20 = mavg (WindowSize 20)

mavg30 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg30 = mavg (WindowSize 30)

mavg50 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg50 = mavg (WindowSize 50)

mavg100 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg100 = mavg (WindowSize 100)


mavgBar :: WindowSize -> Vector (t, Double) -> Vector (t, Double)
mavgBar w@(WindowSize k) vs =
  let (ts, ys) = Vec.unzip vs
      ts' = Vec.drop k ts
      ys' = movingAverage w ys
  in Vec.zip ts' ys'



class MovingAverage vec where
  movingAverage :: WindowSize -> vec Double -> vec Double
  mavg :: WindowSize -> vec Double -> vec (Double, Double)

instance MovingAverage [] where
  movingAverage = movingAvgL
  mavg = mavgL
  
instance MovingAverage Vector where
  movingAverage = movingAvgV
  mavg = mavgV

