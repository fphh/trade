

module Trade.Trade.TradeList where



import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Maybe (isNothing)

import Trade.Timeseries.Algorithm.SyncZip

import Trade.Type.Yield
import Trade.Type.EquityAndShare

import Trade.Trade.PriceSignal
import Trade.Trade.ImpulseSignal
import Trade.Trade.State
import Trade.Trade.SafeTail

data Trade ohlc = Trade {
  tradeState :: State
  , ticker :: Vector (UTCTime, ohlc)
  } deriving (Show)


newtype TradeList ohlc = TradeList {
  unTradeList :: [Trade ohlc]
  } deriving (Show)

impulses2trades :: PriceSignal ohlc -> ImpulseSignal ohlc -> TradeList ohlc
impulses2trades (PriceSignal ps) (ImpulseSignal is) =
  let ss = syncZip ps is
      impulse (_, (_, x)) = x

      go vs | Vec.null vs = []
      go vs =
        let (a, as) = (shead "impulses2trades (1)" vs, stail "impulses2trades" vs)

            (xs, ys) = Vec.span (isNothing . impulse) as
        in Vec.cons a xs : go ys

      us = go (Vec.dropWhile (isNothing . impulse) ss)

      f as bs = Vec.snoc as (shead "impulses2trades (2)" bs)
      ns = zipWith f us (tail us)

      g vs =
        flip Trade (Vec.map (\(a, (b, _)) -> (a, b)) vs)
        $ case Vec.head vs of
            (_, (_, Just Buy)) -> Long
            _ -> NoPosition
          
  in TradeList (map g ns)



data NormTrade ohlc = NormTrade {
  normTradeState :: State
  , normTradeDuration :: NominalDiffTime
  , normedYield :: Vector Yield
  } deriving (Show)


newtype NormTradeList ohlc = NormTradeList {
  unNormTradeList :: [NormTrade ohlc]
  } deriving (Show)



trades2normTrades :: (ToYield ohlc) => TradeList ohlc -> NormTradeList ohlc
trades2normTrades (TradeList tl) =
  let g (_, old) (_, new) = (old `forwardYield` new)
      
      f (Trade st ts) =
        let (t0, _) = shead "trades2normTrades" ts
            (t1, _) = slast "trades2normTrades" ts
            dur = t1 `diffUTCTime` t0
        in NormTrade st dur
           $ Vec.zipWith g ts (stail "trades2normTrades" ts)
           
  in NormTradeList (map f tl)


trade2equity :: (UnOHLC a) => (ohlc -> a) -> Equity -> TradeList ohlc -> Vector (UTCTime, Equity)
trade2equity tradeAt (Equity eqty) (TradeList tl) =
  let p (Trade NoPosition _) = False
      p _ = True

      g yys =
        let (tts, zs) = Vec.unzip yys
        in Vec.cons
           (shead "trade2equity" tts, 1)
           (Vec.zip (stail "trade2equity (1)" tts) (Vec.zipWith (/) (stail "trade2equity (2)" zs) zs))

      unCl (Trade _ vs) = Vec.map (fmap (unOHLC . tradeAt)) vs
      
      cs = map (g . unCl) (filter p tl)

      (ts, ys) = Vec.unzip (Vec.concat cs)

      ysNew = Vec.map Equity (stail "trade2equity" (Vec.scanl (*) eqty ys))
      
  in Vec.zip ts ysNew