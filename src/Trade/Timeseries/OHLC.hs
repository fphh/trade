
module Trade.Timeseries.OHLC where

import Trade.Type.EquityAndShare

data OHLC = OHLC {
  _ohlcOpen :: Open
  , _ohlcHigh :: High
  , _ohlcLow :: Low
  , _ohlcClose :: Close
  , _ohlcVolume :: Volume
  } deriving (Show)


class OHLCInterface a where
  ohlcOpen :: a -> Open
  ohlcHigh :: a -> High
  ohlcLow :: a -> Low
  ohlcClose :: a -> Close
  ohlcVolume :: a -> Volume

instance OHLCInterface OHLC where
  ohlcOpen = _ohlcOpen
  ohlcHigh = _ohlcHigh
  ohlcLow = _ohlcLow
  ohlcClose = _ohlcClose
  ohlcVolume = _ohlcVolume
