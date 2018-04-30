

module Trade.Trade.BuyAndSell where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Timeseries.Algorithm.Intersection (Intersection(..))

import Trade.Type.EquityAndShare (Close(..), Share(..))

data BuySell = Buy | Sell | Hold deriving (Show, Eq, Ord)

tradeSignal :: Intersection -> BuySell
tradeSignal Down = Buy
tradeSignal Up = Sell
tradeSignal NoIntersection = Hold


data Trade a = Trade {
  tradeTime :: UTCTime
  , tradeTrade :: BuySell
  , tradeShare :: Share
  }
  
{-

buyAndSell ::
  Vector (UTCTime, BuySell)
  -> Vector (UTCTime, Close)
  -> Vector (UTCTime, BuySell, Equity, Share, Close)
buyAndSell bs vs =
  let equity = Equity 1000
      shares = Share 0

      ss = syncZip bs vs

      (start, (sig, pps)) = Vec.head ss
      
  in Vec.reverse
     $ Vec.fromList
     $ Vec.foldl' trade [(start, sig, equity, shares, pps)] ss


buyAndSell ::
  Vector (UTCTime, BuySell)
  -> Vector (UTCTime, Close)
  -> Vector (UTCTime, BuySell, Equity, Share, Close)
buyAndSell bs vs =
  let equity = Equity 1000
      shares = Share 0

      ss = syncZip bs vs

      (start, (sig, pps)) = Vec.head ss
      
  in Vec.reverse
     $ Vec.fromList
     $ Vec.foldl' trade [(start, sig, equity, shares, pps)] ss
-}
