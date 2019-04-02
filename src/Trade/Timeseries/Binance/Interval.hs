

module Trade.Timeseries.Binance.Interval where

import qualified Data.List as List

import Trade.Timeseries.Url

import Trade.Type.Bars (BarLength(..))

newtype Interval = Interval {
  unInterval :: BarLength
  } deriving (Eq, Show)

instance ToUrl Interval where
  toUrl (Interval i) =
    let is =
          [ (Min 1, "1m"), (Min 3, "3m"), (Min 5, "5m"), (Min 15, "15m"), (Min 30, "30m")
          , (Hour 1, "1h"), (Hour 2, "2h"), (Hour 4, "4h"), (Hour 6, "6h"), (Hour 8, "8h"), (Hour 12, "12h")
          , (Day 1, "1d"), (Day 3, "3d")
          , (Week 1, "1w"), (Month 1, "1M") ]
        err = error $
          "binance does not support interval '" ++ show i ++ "'\n"
          ++ "Supported intervals are:\n" ++ List.intercalate "\n" (map (('\t':) . show . fst) is)
    in maybe err id (List.lookup i is)

