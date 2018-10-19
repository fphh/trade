{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module Trade.Timeseries.Row where

import Data.Time.Clock (UTCTime)

import Trade.Type.OHLC (Open, Close, High, Low, Volume)

import Trade.Timeseries.OHLC

class RowInterface row where
  dateR :: row -> UTCTime
  openR :: row -> Maybe Open
  highR :: row -> Maybe High
  lowR :: row -> Maybe Low
  closeR :: row -> Maybe Close
  volumeR :: row -> Maybe Volume


class DateInterface row where
  type TyD row :: *
  type TyR row :: *
  dateDI :: row -> TyD row
  removeDI :: row -> TyR row

instance DateInterface (t, a) where
  type TyD (t, a) = t
  type TyR (t, a) = a
  dateDI = fst
  removeDI = snd

instance DateInterface (t, a, b) where
  type TyD (t, a, b) = t
  type TyR (t, a, b) = (a, b)
  dateDI (t, _, _) = t
  removeDI (_, x, y) = (x, y)


isIncomplete :: (RowInterface row) => row -> Bool
isIncomplete r =
  let b = openR r >> highR r >> lowR r >> closeR r >> volumeR r
  in b /= Nothing



prepareData :: (RowInterface row) => row -> OHLC
prepareData r = OHLC {
  _ohlcOpen = maybe (error "") id (openR r)
  , _ohlcHigh = maybe (error "") id (highR r)
  , _ohlcLow = maybe (error "") id (lowR r)
  , _ohlcClose = maybe (error "") id (closeR r)
  , _ohlcVolume = maybe (error "") id (volumeR r)
  }
