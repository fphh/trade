{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}


module Trade.Trade.BuyAndSell where

import Prelude hiding (div)

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Timeseries.Algorithm.Intersection (Intersection(..))
import Trade.Timeseries.Algorithm.SyncZip

import Trade.Type.EquityAndShare -- (Close(..), Share(..), Equity(..))

data BuySellSignal = Buy | Sell | Hold deriving (Show, Eq, Ord)

data BuySellTrade = BuyTrade | SellTrade deriving (Show, Eq, Ord)

data TradeOrNot = DoTrade | DoNotTrade deriving (Show, Eq, Ord)

data Trade a = Trade {
  tradeTrade :: BuySellTrade
  , tradeShare :: Share
  } deriving (Show)

data Portfolio a = Portfolio {
  equity :: Equity
  , shares :: Share
  } deriving (Show)

newtype TradeSignal a = TradeSignal (Vector (UTCTime, BuySellSignal)) deriving (Show)

newtype PriceSignal a = PriceSignal (Vector (UTCTime, a)) deriving (Show)

newtype EquitySignal a = EquitySignal (Vector (UTCTime, Portfolio a)) deriving (Show)

newtype TradeList a = TradeList (Vector (UTCTime, Trade a)) deriving (Show)


toTradeSignal ::
  (Intersection -> BuySellSignal)
  -> Vector (UTCTime, Intersection)
  -> TradeSignal a
toTradeSignal f vs = TradeSignal (Vec.map (fmap f) vs)


trade :: (Div a, Mult a) => (Portfolio a, [(UTCTime, Trade a)]) -> (UTCTime, (BuySellSignal, a)) -> (Portfolio a, [(UTCTime, Trade a)])
trade (Portfolio eqty shrs, acc) (t, (Buy, pps)) =
  let eqty' = eqty - shrs' `mult` pps
      shrs' = eqty `div` pps
  in (Portfolio eqty' shrs', (t, Trade BuyTrade shrs') : acc)
trade (Portfolio eqty shrs, acc) (t, (Sell, pps)) =
  let eqty' = eqty + shrs `mult` pps
      shrs' = shrs - eqty' `div` pps
  in (Portfolio eqty' shrs', (t, Trade SellTrade shrs) : acc)
trade acc (_, (Hold, _)) = acc


toTrades :: (Div a, Mult a) => Portfolio a -> TradeSignal a -> PriceSignal a -> TradeList a
toTrades portfolio (TradeSignal bs) (PriceSignal vs) =
  TradeList
  $ Vec.reverse
  $ Vec.fromList
  $ snd
  $ Vec.foldl' trade (portfolio, []) (syncZip bs vs)


tradesToEquity ::(Mult a) =>  Portfolio a -> TradeList a -> PriceSignal a -> EquitySignal a
tradesToEquity portfolio (TradeList tl) (PriceSignal vs) =
  let f (pf@(Portfolio eqty shrs), acc) (t, (Trade SellTrade s, pps)) =
        let eqty' = eqty + s `mult` pps
            shrs' = shrs - s
        in (Portfolio eqty' shrs', (t, pf):acc)
      f (pf@(Portfolio eqty shrs), acc) (t, (Trade BuyTrade s, pps)) =
        let eqty' = eqty - s `mult` pps
            shrs' = shrs + s
        in (Portfolio eqty' shrs', (t, pf):acc)
  in EquitySignal
     $ Vec.reverse
     $ Vec.fromList
     $ snd --  (uncurry (:))
     $ Vec.foldl' f (portfolio, []) (syncZip tl vs)

{-
trade ::
  [(a, BuySell, Equity, Share, Close)]
  -> (a, (BuySell, Close))
  -> [(a, BuySell, Equity, Share, Close)]
trade acc@((_, _, e, s, _):_) (t, (Buy, pps)) =
  let s' = e ./ pps
      e' = e - s' .* pps
  in (t, Buy, e', s', pps):acc
trade acc@((_, _, e, s, _):_) (t, (Sell, pps)) =
  let e' = e + s .* pps
  in (t, Sell, e', 0, pps):acc
trade acc@((_, _, e, s, _):_) (t, (Hold, pps)) = (t, Hold, e, s, pps):acc
trade acc _ = acc
-}

{-
buyAndSell ::
  TradeSignal a
  -> Vector (UTCTime, a)
  -> Vector (Trade a)
buyAndSell bs vs =
  let equity = Equity 1000
      shares = Share 0

      ss = syncZip bs vs

      (start, (sig, pps)) = Vec.head ss
      
  in Vec.reverse
     $ Vec.fromList
     -- $ Vec.foldl' trade [(start, sig, equity, shares, pps)] ss
     $ Vec.foldl' trade [Trade start sig shares] ss
-}
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
