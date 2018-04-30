{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Timeseries.Quandl.Row where

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
  dateDI :: row -> UTCTime

instance DateInterface (UTCTime, a) where
    dateDI = fst

instance DateInterface (UTCTime, a, b) where
    dateDI (t, _, _) = t

