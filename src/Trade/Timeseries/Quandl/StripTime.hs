{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.Timeseries.Quandl.StripTime where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

class StripTime row where
  type Ty row :: *
  stripTime :: row -> Ty row

instance StripTime (UTCTime, a) where
  type Ty (UTCTime, a) = a
  stripTime = snd

instance StripTime (UTCTime, a, b) where
  type Ty (UTCTime, a, b) = (a, b)
  stripTime (_, a, b) = (a, b)

instance (StripTime a) => StripTime (Vector a) where
  type Ty (Vector a) = Vector (Ty a)
  stripTime = Vec.map stripTime

instance (StripTime a) => StripTime [a] where
  type Ty [a] = [Ty a]
  stripTime = map stripTime


