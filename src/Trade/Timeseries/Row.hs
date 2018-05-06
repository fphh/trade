{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module Trade.Timeseries.Row where

import Data.Time.Clock (UTCTime)

import Trade.Type.EquityAndShare (Open, Close, High, Low, Volume)

class RowInterface row where
  dateR :: row -> UTCTime
  openR :: row -> Maybe Open
  highR :: row -> Maybe High
  lowR :: row -> Maybe Low
  closeR :: row -> Maybe Close
  volumeR :: row -> Maybe Volume


class DateInterface row where
  type Ty row :: *
  dateDI :: row -> UTCTime
  removeDI :: row -> Ty row

instance DateInterface (UTCTime, a) where
  type Ty (UTCTime, a) = a
  dateDI = fst
  removeDI = snd

instance DateInterface (UTCTime, a, b) where
  type Ty (UTCTime, a, b) = (a, b)
  dateDI (t, _, _) = t
  removeDI (_, x, y) = (x, y)

