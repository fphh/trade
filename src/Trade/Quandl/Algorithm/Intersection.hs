

module Trade.Quandl.Algorithm.Intersection where

import Data.Time.Clock (UTCTime)


import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Quandl.Algorithm.SyncZip (syncZip)


-- import Debug.Trace

--    y1 = m x1 + b und y2 = n x2 + c.
--    x = (c−b) / (m−n)
--    y = m*x + b


intersect :: (Show b, Fractional b) => b -> b -> b -> b -> (b, b)
intersect b0 b1 y0 y1 =
  let m = b1-b0
      n = y1-y0
      x = (y0-b0) / (m-n)
      y = m*x + b0
  in (x, y)

data Intersection = Up | Down | NoIntersection deriving (Eq, Show)

intersection :: Vector (UTCTime, Double) -> Vector (UTCTime, Double) -> Vector (UTCTime, Intersection)
intersection vs us =
  let ss = syncZip vs us
  
      f (_, (y0, y0')) (x1, (y1, y1')) =
        let (x, _) = intersect y0 y1 y0' y1'
        in case x of
             r | r <= 0 -> (x1, NoIntersection)
             r | r >= 1 -> (x1, NoIntersection)
             _ -> (x1, if (y0 < y0') then Up else Down)
  in Vec.zipWith f ss (Vec.tail ss)


data IntersectionArgs = IntersectionArgs {
  middle :: Double
  , high :: Double
  , low :: Double
  } deriving (Show)

intersectionToLine :: IntersectionArgs -> Vector (UTCTime, Intersection) -> Vector (UTCTime, Double)
intersectionToLine args =
  let m = middle args
      l = low args
      h = high args
      f (t, Up) = [(t, m), (t, l), (t, m)]
      f (t, Down) = [(t, m), (t, h), (t, m)]
      f _ = []
  in Vec.fromList . concatMap f . Vec.toList

