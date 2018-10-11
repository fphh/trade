

module Trade.Test.Data where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Timeseries.Time

import Trade.Test.Time (jan, feb, mar, apr, may)

import Debug.Trace

test1 :: Vector (UTCTime, Double)
test1 =
  let up = [1 .. 17]
      down = [18, 17 .. 5]
      up2 = [4 .. 14]
      down2 = [15, 14 .. 2]
      -- up3 = [3 .. 20]
      -- down3 = [21, 20 .. 1]


      
      ys = up ++ down -- ++ up2 ++ down2 -- ++ up3 ++ down3
  in Vec.zip (Vec.concat [jan, feb, mar, apr, may]) (Vec.fromList ys)

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
  in Vec.zip (Vec.concat [jan, feb, mar, apr, may]) (Vec.fromList ys)


test_up_down :: Vector (UTCTime, Double)
test_up_down =
  let ys = take (Vec.length jan) (cycle [1, 2])
  in Vec.zip jan (Vec.fromList ys)



test3 :: Vector (UTCTime, Double)
test3 =
  let up = [5 .. 15]
      down = [15, 14 .. 1]
      up2 = [1 .. 10]
      down2 = [10, 9 .. 2]


      
      ys = down ++ up2 ++ down2 ++ up2 -- ++ down2 -- ++ up3 ++ down3
  in Vec.zip (Vec.concat [jan, feb, mar, apr, may]) (Vec.fromList ys)
