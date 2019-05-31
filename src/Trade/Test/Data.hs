

module Trade.Test.Data where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Test.Time (jan2017, feb2017, mar2017, apr2017, may2017)


test1 :: Vector (UTCTime, Double)
test1 =
  let up = [1 .. 17]
      down = [18, 17 .. 5]
      up2 = [4 .. 14]
      down2 = [15, 14 .. 2]
      up3 = [3 .. 20]
      down3 = [21, 20 .. 1]


      
      ys = up ++ down ++ up2 ++ down2 ++ up3 ++ down3
  in Vec.zip (Vec.concat [jan2017, feb2017, mar2017, apr2017, may2017]) (Vec.fromList ys)

test2 :: Vector (UTCTime, Double)
test2 =
  let up = [1 .. 17]
      down = [18, 17 .. 5]
      up2 = [4 .. 14]
      down2 = [15, 14 .. 2]
      up3 = [3 .. 20]
      down3 = [21, 20 .. 1]
      up4 = [ 2 .. 17 ]
      down4 = [ 18, 17 .. 10]
      
      ys = up ++ down ++ up2 ++ down2 ++ up3 ++ down3 ++ up4 ++ down4
  in Vec.zip (Vec.concat [jan2017, feb2017, mar2017, apr2017, may2017]) (Vec.fromList ys)


test_up_down :: Vector (UTCTime, Double)
test_up_down =
  let ys = take (Vec.length jan2017) (cycle [1::Double, 2])
  in Vec.zip jan2017 (Vec.fromList ys)



test3 :: Vector (UTCTime, Double)
test3 =
  let down = [15 , 14 .. 1]
      up2 = [1 .. 10]
      down2 = [10, 9 .. 2]
      ys = down ++ up2 ++ down2 ++ up2
  in Vec.zip (Vec.concat [jan2017, feb2017, mar2017, apr2017, may2017]) (Vec.fromList ys)


test4 :: Vector (UTCTime, Double)
test4 =
  let ys = [6, 7, 8, 9]
      zs = [10, 9, 8, 7]
  in Vec.zip (Vec.concat [jan2017, feb2017, mar2017, apr2017, may2017]) (Vec.fromList (ys++zs++ys++zs++ys++zs++ys++zs++ys++zs))


testSimple :: Vector (UTCTime, Double)
testSimple =
  let ys = [30, 20,40,60,80,100,80,60,40,20]
  in Vec.zip jan2017 (Vec.fromList ys)


sinus :: Vector (UTCTime, Double)
sinus =
  let ys = map ((2+) . sin) [0, 0.2 .. ]
  in Vec.zip (Vec.concat [jan2017, feb2017, mar2017, apr2017, may2017]) (Vec.fromList ys)


linear :: Vector (UTCTime, Double)
linear =
  let ys = [1, 2 .. ]
  in Vec.zip (Vec.concat [jan2017, feb2017, mar2017, apr2017, may2017]) (Vec.fromList ys)
