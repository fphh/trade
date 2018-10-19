

module Trade.Algorithm.MovingAverage where

import Statistics.Sample (mean)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.List (scanl')

class MovingAverage vec where
  movingAverage :: Int -> vec Double -> vec Double
  mavg :: Int -> vec Double -> vec (Double, Double)


movingAvgL :: Int -> [Double] -> [Double]
movingAvgL k lst =
  let (h, t) = splitAt k lst 
  in map (/ fromIntegral k) $ scanl' (+) (sum h) $ zipWith (-) t lst


mavgL :: Int -> [Double] -> [(Double, Double)]
mavgL k xs =
  let j = fromIntegral k -- / 2
  in zip [j, j+1 ..] (movingAvgL k xs)


instance MovingAverage [] where
  movingAverage = movingAvgL
  mavg = mavgL
  

movingAvgV :: Int -> Vector Double -> Vector Double
movingAvgV k v =
  let len = Vec.length v
      f a = Vec.slice a k v
      idx = Vec.generate (len-k) id
  in Vec.map (mean . f) idx

mavgV :: Int -> Vector Double -> Vector (Double, Double)
mavgV k v =
  let len = Vec.length v
      f a = (fromIntegral (k+a), mean (Vec.slice a k v))
  in Vec.generate (len-k) f


instance MovingAverage Vector where
  movingAverage = movingAvgV
  mavg = mavgV


mavg3 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg3 = mavg 3

mavg5 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg5 = mavg 5

mavg8 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg8 = mavg 8

mavg12 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg12 = mavg 12

mavg15 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg15 = mavg 15

mavg20 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg20 = mavg 20

mavg30 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg30 = mavg 30

mavg50 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg50 = mavg 50

mavg100 :: MovingAverage vec => vec Double -> vec (Double, Double)
mavg100 = mavg 100


mavgBar :: Int -> Vector (t, Double) -> Vector (t, Double)
mavgBar n vs =
  let (ts, ys) = Vec.unzip vs
      ts' = Vec.drop n ts
      ys' = movingAverage n ys
  in Vec.zip ts' ys'

