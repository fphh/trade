{-# LANGUAGE TypeFamilies #-}

module Trade.Timeseries.Quandl.CBOE where

import Data.Time.Clock (UTCTime)

import Data.Csv (FromNamedRecord, parseNamedRecord)

import Trade.Timeseries.Quandl.Database
import Trade.Timeseries.Quandl.Time ()
import Trade.Timeseries.Quandl.Helper

-- "Date","Total Call Volume","Total Put Volume","Total Option Volume","P/C Ratio"

-- "Trade Date","Open","High","Low","Close","Settle","Change","Total Volume","EFP","Prev. Day Open Interest" 


data Row = Row {
  tradeDate :: !UTCTime
  , open :: !Double
  , high :: !Double
  , low :: !Double
  , close :: !Double
  , settle :: !Double
  , change :: !Double
  , totalVolume :: !Double
  , eFP :: !Double
  , prevDayOpenInterest :: !Double 
  } deriving (Show)


instance FromNamedRecord Row where
  parseNamedRecord r =
    Row
    <$> (r .:: "Trade Date")
    <*> (r .:: "Open")
    <*> (r .:: "High")
    <*> (r .:: "Low")
    <*> (r .:: "Close")
    <*> (r .:: "Settle")
    <*> (r .:: "Change")
    <*> (r .:: "Total Volume")
    <*> (r .:: "EFP")
    <*> (r .:: "Prev. Day Open Interest")


instance ToRow CBOE where
  type RowTy CBOE = Row


instance FromDatabase CBOE where
  type Dataset CBOE = CBOECode

data CBOE = CBOE deriving (Show)

data CBOECode =
  TOTAL_PC
  | INDEX_PC
  | VXK2004
  deriving (Show)
