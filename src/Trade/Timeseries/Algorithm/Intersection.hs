

module Trade.Timeseries.Algorithm.Intersection where


import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Help.SafeTail

import Trade.Timeseries.Algorithm.SyncZip (syncZip)

import Trade.Report.Pretty


--    y1 = m x1 + b und y2 = n x2 + c.
--    x = (c−b) / (m−n)
--    y = m*x + b


intersect :: (Fractional b) => b -> b -> b -> b -> (b, b)
intersect b0 b1 y0 y1 =
  let m = b1-b0
      n = y1-y0
      x = (y0-b0) / (m-n)
      y = m*x + b0
  in (x, y)

data Intersection = Up | Down | NoIntersection deriving (Eq, Show)

instance Pretty Intersection where
  pretty = show

intersection :: (Ord t) => Vector (t, Double) -> Vector (t, Double) -> Vector (t, Intersection)
intersection vs us =
  let ss = syncZip vs us
  
      f (_, (y0, y0')) (x1, (y1, y1')) =
        let (x, _) = intersect y0 y1 y0' y1'
        in case x of
             r | r <= 0 -> (x1, NoIntersection)
             r | r > 1 -> (x1, NoIntersection)
             _ -> (x1, if (y0 < y0') then Up else Down)
  in Vec.zipWith f ss (stail "intersection" ss)

