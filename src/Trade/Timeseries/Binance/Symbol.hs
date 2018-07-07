{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Trade.Timeseries.Binance.Symbol where

import GHC.Generics

import qualified Data.Text as Text

import qualified Data.Vector as Vec

import qualified Data.Aeson as Aeson

import Data.Scientific

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX

import Trade.Timeseries.Url
import Trade.Timeseries.Row

import Trade.Type.OHLC (Open(..), High(..), Low(..), Close(..), Volume(..))


data Row = Row {
  fromDate :: !UTCTime
  , toDate :: !UTCTime
  , open :: !Open
  , high :: !High
  , low :: !Low
  , close :: !Close
  , volume :: !Volume
  } deriving (Show, Generic)

millisToUTC :: Integer -> UTCTime
millisToUTC t = posixSecondsToUTCTime $ (fromInteger t) / 1000

instance Aeson.FromJSON Row where
  parseJSON (Aeson.Array as) =
    let Aeson.Number from2 = as Vec.! 0
        Aeson.String o = as Vec.! 1
        Aeson.String h = as Vec.! 2
        Aeson.String l = as Vec.! 3
        Aeson.String c = as Vec.! 4
        Aeson.String v = as Vec.! 5
        Aeson.Number to2 = as Vec.! 6
    in pure $ Row {
      fromDate = either (error "Binance.Symbol: from") millisToUTC (floatingOrInteger from2 :: Either Double Integer)
      , toDate = either (error "Binance.Symbol: to") millisToUTC (floatingOrInteger to2 :: Either Double Integer)
      , open = Open (read $ Text.unpack o)
      , high = High (read $ Text.unpack h)
      , low = Low (read $ Text.unpack l)
      , close = Close (read $ Text.unpack c)
      , volume = Volume (read $ Text.unpack v)
      }
  parseJSON _ = error "Binance.Symbol: FromJSON not an array"

instance RowInterface Row where
  dateR = toDate
  openR = Just . open
  highR = Just . high
  lowR = Just . low
  closeR = Just . close
  volumeR = Just . volume

instance DateInterface Row where
  type Ty Row = Row
  dateDI = toDate
  removeDI = id

instance ToUrl Symbol where
  toUrl = show


data Symbol =
  XMRBTC
  | BTCUSDT
  | BCCBTC
  | BCCUSDT
  | BNBUSDT
  | XRPUSDT
  deriving (Show, Read)

