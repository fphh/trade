{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Trade.BuyAndSell where

import Prelude hiding (div)

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Timeseries.Algorithm.Intersection (Intersection(..))
import Trade.Timeseries.Algorithm.SyncZip

import Trade.Report.Report (ToNumberedList, toNumberedList, ToNumberedLine, toNumberedLine)

import Trade.Type.EquityAndShare -- (Close(..), Share(..), Equity(..))

data TradeType = Buy | Sell | Hold deriving (Show, Eq, Ord)

data BuySellTrade = BuyTrade | SellTrade deriving (Show, Eq, Ord)

data TradeOrNot = DoTrade | DoNotTrade deriving (Show, Eq, Ord)

data Trade a = Trade {
  tradeTrade :: BuySellTrade
  , tradeShare :: Share
  } deriving (Show)

data Portfolio = Portfolio {
  equity :: Equity
  , shares :: Share
  } deriving (Show)

newtype TradeSignal ohcl = TradeSignal (Vector (UTCTime, TradeType)) deriving (Show)

newtype PriceSignal ohcl = PriceSignal (Vector (UTCTime, ohcl)) deriving (Show)

newtype EquitySignal = EquitySignal (Vector (UTCTime, Portfolio)) deriving (Show)

newtype TradeList ohcl = TradeList (Vector (UTCTime, Trade ohcl)) deriving (Show)


toTradeSignal ::
  (Intersection -> TradeType)
  -> Vector (UTCTime, Intersection)
  -> TradeSignal ohcl
toTradeSignal f vs = TradeSignal (Vec.map (fmap f) vs)


trade ::
  (Div ohlc, Mult ohlc) =>
  (Portfolio, [(UTCTime, Trade ohlc)]) -> (UTCTime, (TradeType, ohlc)) -> (Portfolio, [(UTCTime, Trade ohlc)])
trade (Portfolio eqty shrs, acc) (t, (Buy, pps)) =
  let eqty' = eqty - shrs' `mult` pps
      shrs' = eqty `div` pps
  in (Portfolio eqty' shrs', (t, Trade BuyTrade shrs') : acc)
trade (Portfolio eqty shrs, acc) (t, (Sell, pps)) =
  let eqty' = eqty + shrs `mult` pps
      shrs' = shrs - eqty' `div` pps
  in (Portfolio eqty' shrs', (t, Trade SellTrade shrs) : acc)
trade acc (_, (Hold, _)) = acc


toTrades ::
  (Div ohlc, Mult ohlc) =>
  Portfolio -> TradeSignal ohlc -> PriceSignal ohlc -> TradeList ohlc
toTrades portfolio (TradeSignal bs) (PriceSignal vs) =
  TradeList
  $ Vec.reverse
  $ Vec.fromList
  $ snd
  $ Vec.foldl' trade (portfolio, []) (syncZip bs vs)


tradesToEquity :: (Mult ohlc) => Portfolio -> TradeList ohlc -> PriceSignal ohlc -> EquitySignal
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
     $ snd
     $ Vec.foldl' f (portfolio, []) (syncZip tl vs)


filterHold :: TradeSignal ohlc -> TradeSignal ohlc
filterHold (TradeSignal ts) =
  let p (_, Hold) = False
      p _ = True
  in TradeSignal (Vec.filter p ts)

newtype HoldingTime = HoldingTime { unHoldingTime :: NominalDiffTime } deriving (Show)

data PositionType =
  Long
  | Short
  | NoPosition
  deriving (Show)


data Position ohlc = Position {
  positionType:: PositionType
  , holdingTime :: HoldingTime
  , outToInRatio :: OutToInRatio ohlc
  } deriving (Show)

newtype StateList ohlc = StateList (Vector (Position ohlc)) deriving (Show)

instance (Show ohlc) => ToNumberedLine (Position ohlc) where
  toNumberedLine i (Position ptype (HoldingTime t) (OutToInRatio w)) =
    [show i, show ptype, show t, show w]

instance (ToNumberedLine (Position ohlc)) => ToNumberedList (StateList ohlc) where
  toNumberedList (StateList xs) = toNumberedList xs


stateListWitMaximalTrades ::
  (ToRatio ohlc, Show ohlc) =>
  TradeSignal ohlc -> PriceSignal ohlc -> StateList ohlc
stateListWitMaximalTrades (TradeSignal tl) (PriceSignal vs) =
  let ss = syncZip tl vs
      f (t0, (Sell, pps0)) (t1, (Buy, pps1)) =
        Position NoPosition (HoldingTime (t1 `diffUTCTime` t0)) (pps1 ./ pps0)
      f (t0, (Buy, pps0)) (t1, (Sell, pps1)) =
        Position Long (HoldingTime (t1 `diffUTCTime` t0)) (pps1 ./ pps0)
  in StateList (Vec.zipWith f ss (Vec.tail ss))



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
