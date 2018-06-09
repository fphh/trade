

module Trade.Timeseries.Url where

class ToUrl a where
  toUrl :: a -> String
