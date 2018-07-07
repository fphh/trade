

module Trade.Test.Data where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Text.Printf (printf)

import Trade.Timeseries.Time

import Debug.Trace

jan = map ((\(Just t) -> t) . parseDate . ("2019-01-"++) . printf "%02d") [1 :: Integer .. 31]
feb = map ((\(Just t) -> t) . parseDate . ("2019-02-"++) . printf "%02d") [1 :: Integer .. 28]
mar = map ((\(Just t) -> t) . parseDate . ("2019-03-"++) . printf "%02d") [1 :: Integer .. 31]
apr = map ((\(Just t) -> t) . parseDate . ("2019-04-"++) . printf "%02d") [1 :: Integer .. 30]
may = map ((\(Just t) -> t) . parseDate . ("2019-05-"++) . printf "%02d") [1 :: Integer .. 31]

test1 :: Vector (UTCTime, Double)
test1 =
  let up = [1 .. 17]
      down = [18, 17 .. 5]
      up2 = [4 .. 14]
      down2 = [15, 14 .. 2]
      -- up3 = [3 .. 20]
      -- down3 = [21, 20 .. 1]


      
      ys = up ++ down -- ++ up2 ++ down2 -- ++ up3 ++ down3
  in Vec.zip (Vec.fromList (jan++feb++mar++apr++may)) (Vec.fromList ys)

test2 :: Vector (UTCTime, Double)
test2 =
  let up = [10 .. 17]
      down = [18, 17 .. 5]
      up2 = [4 .. 14]
      down2 = [15, 14 .. 5]
      up3 = [4 .. 20]
      down3 = [21, 20 .. 8]
      up4 = [ 7 .. 17 ]
      down4 = [ 18, 17 .. 10]
      
      ys = up ++ down ++ up2 ++ down2 ++ up3 ++ down3 ++ up4 ++ down4
  in Vec.zip (Vec.fromList (jan++feb++mar++apr++may)) (Vec.fromList ys)



test3 :: Vector (UTCTime, Double)
test3 =
  let up = [5 .. 15]
      down = [15, 14 .. 1]
      up2 = [1 .. 10]
      down2 = [10, 9 .. 2]


      
      ys = down ++ up2 ++ down2 ++ up2 -- ++ down2 -- ++ up3 ++ down3
  in Vec.zip (Vec.fromList (jan++feb++mar++apr++may)) (Vec.fromList ys)
