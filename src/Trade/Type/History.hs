{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.Type.History where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Trade.Type.Bars (BarNo(..))
import Trade.Type.Yield (Yield(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.StepFunc (StepFunc)

import Trade.Help.SafeTail (shead, stail)

import Trade.Report.Curve

-- | History by `BarNo`, usually `Equity` or `Yield`.
newtype History p = History {
  unHistory :: Vector (BarNo, p)
  } deriving (Show, Eq)

instance Functor History where
  fmap f (History h) = History (Vec.map (fmap f) h)


instance Curve (History Yield) where
  type Ty (History Yield) = Int
  curve = fmap (\(BarNo x, Yield y) -> (x, y)) . unHistory

instance Curve (History Equity) where
  type Ty (History Equity) = Int
  curve = fmap (\(BarNo x, Equity y) -> (x, y)) . unHistory


-- | TODO: Testing (yield2equity . equity2yield) == id
yield2equity :: StepFunc -> Equity -> History Yield -> History Equity
yield2equity step eqty (History hist) =
  let f (_, e) (t1, y) = (t1, step e y)
      (t0, y0) = shead "normHistory2normEquity" hist
  in History (Vec.scanl' f (t0, step eqty y0) (stail "yield2equity" hist))

equity2yield :: History Equity -> History Yield
equity2yield (History hist) =
  let g (_, Equity old) (b, Equity new) = (b, Yield (new / old))
      start = fmap (const (Yield 1)) (Vec.head hist)
  in History (Vec.cons start (Vec.zipWith g hist (stail "equity2yield" hist)))

