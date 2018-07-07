

module Trade.Timeseries.Binance.Interval where

import Trade.Timeseries.Url


data Interval =
  Min1
  | Min3
  | Min5
  | Min15
  | Min30
  | Hour1
  | Hour2
  | Hour4
  | Hour6
  | Hour8
  | Hour12
  | Day1
  | Day3
  | Week1
  | Month1
  

instance ToUrl Interval where
  toUrl i =
    case i of
      Min1 -> "1m"
      Min3 -> "3m"
      Min5 -> "5m"
      Min15 -> "15m"
      Min30 -> "30m"
      Hour1 -> "1h"
      Hour2 -> "2h"
      Hour4 -> "4h"
      Hour6 -> "6h"
      Hour8 -> "8h"
      Hour12 -> "12h"
      Day1 -> "1d"
      Day3 -> "3d"
      Week1 -> "1w"
      Month1 -> "1M"

  
