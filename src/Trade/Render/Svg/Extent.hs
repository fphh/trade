{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Svg.Extent where

import Data.Time.Clock


import Trade.Svg.Layout
import Trade.Svg.DrawingVec

--import Debug.Trace

data XExtent x = XExtent { xmin :: !x, xmax :: !x } deriving  (Show)

data YExtent = YExtent { ymin :: !Double, ymax :: !Double } deriving  (Show)
 

xyExtent :: (DrawingVec vec, Ord x) => vec (x, Double) -> (XExtent x, YExtent)
xyExtent cs =
  let hi = -1/0
      lo = 1/0
      (v, _) = vecHead cs
      f (XExtent xminv xmaxv, YExtent yminv ymaxv) (x, y) =
        (XExtent (min xminv x) (max xmaxv x), YExtent (min yminv y) (max ymaxv y))
  in vecFoldl f (XExtent v v, YExtent lo hi) cs


class Scale ext where
  type Orient ext :: *
  type Ty ext :: *

  scale :: ext -> Orient ext -> (Ty ext -> Double)

instance Scale (XExtent Double) where
  type Orient (XExtent Double) = InnerWidth
  type Ty (XExtent Double) = Double

  scale (XExtent xminv xmaxv) (InnerWidth w) =
    let a = w / (xmaxv - xminv)
    in \x -> a*(x-xminv)


instance Scale (XExtent UTCTime) where
  type Orient (XExtent UTCTime) = InnerWidth
  type Ty (XExtent UTCTime) = UTCTime

  scale (XExtent start end) (InnerWidth w) =
    let dt = nominalDiffTimeToSeconds (end `diffUTCTime` start)
        m = w / realToFrac dt
    in \t -> let t' = nominalDiffTimeToSeconds (t `diffUTCTime` start) in m * realToFrac t'


instance Scale YExtent where
  type Orient YExtent = InnerHeight
  type Ty YExtent = Double

  scale (YExtent yminv ymaxv) (InnerHeight h) =
    let a = h / (ymaxv - yminv)
    in \y -> h - a*(y - yminv)
