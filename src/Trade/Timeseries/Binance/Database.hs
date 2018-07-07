{-# LANGUAGE RecordWildCards #-}


module Trade.Timeseries.Binance.Database where

import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Trade.Timeseries.Binance.Interval
import Trade.Timeseries.Binance.Symbol

import Trade.Timeseries.Url



data RequestParams = RequestParams {
  baseUrl :: String
  , symbol :: Symbol
  -- , apiKey :: Maybe String
  , interval :: Interval
  , limit :: Maybe Int
  , from :: Maybe UTCTime
  , to :: Maybe UTCTime
  }



utcToMillis :: UTCTime -> Integer
utcToMillis = (1000*) . round . utcTimeToPOSIXSeconds

instance ToUrl RequestParams where
  toUrl RequestParams{..} =
    baseUrl
    ++ "?symbol=" ++ toUrl symbol
    ++ "&interval=" ++ toUrl interval
    ++ maybe "" (\d -> "&limit=" ++ show d) limit
    ++ maybe "" (\d -> "&startTime=" ++ show (utcToMillis d)) from
    ++ maybe "" (\d -> "&endTime=" ++ show (utcToMillis d)) to
