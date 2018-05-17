

module Trade.Trade.TradeList where



import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Maybe (isNothing)

import Trade.Timeseries.Algorithm.SyncZip

import Trade.Type.Yield 

import Trade.Trade.PriceSignal
import Trade.Trade.ImpulseSignal
import Trade.Trade.State

data Trade ohlc = Trade {
  tradeState :: State
  , ticker :: Vector (UTCTime, ohlc)
  } deriving (Show)


newtype TradeList ohlc = TradeList {
  unTradeList :: [Trade ohlc]
  } deriving (Show)


destr :: Vector a -> Maybe (a, Vector a)
destr vs | Vec.null vs = Nothing
destr vs = Just (vs Vec.! 0, Vec.tail vs)


impulses2trades :: PriceSignal ohlc -> ImpulseSignal ohlc -> TradeList ohlc
impulses2trades (PriceSignal ps) (ImpulseSignal is) =
  let ss = syncZip ps is
      impulse (_, (_, x)) = x

      go vs | Vec.null vs = []
      go vs =
        let (a, as) = (Vec.head vs, Vec.tail vs)
            (xs, ys) = Vec.span (isNothing . impulse) as
        in Vec.cons a xs : go ys

      us = go (Vec.dropWhile (isNothing . impulse) ss)

      f as bs = Vec.snoc as (Vec.head bs)
      ns = zipWith f us (tail us)

      g vs =
        flip Trade (Vec.map (\(a, (b, _)) -> (a, b)) vs)
        $ case Vec.head vs of
            (_, (_, Just Buy)) -> Long
            _ -> NoPosition
          
  in TradeList (map g ns)



data NormTrade ohlc = NormTrade {
  normTradeState :: State
  , normedYield :: Vector (UTCTime, Yield)
  } deriving (Show)


newtype NormTradeList ohlc = NormTradeList {
  unNormTradeList :: [NormTrade ohlc]
  } deriving (Show)



trades2normTrades :: (ToYield ohlc) => TradeList ohlc -> NormTradeList ohlc
trades2normTrades (TradeList tl) =
  let f (Trade st ts) =
        let (_, x) = Vec.head ts
        in NormTrade st (Vec.map (fmap (`forwardYield` x)) ts)
  in NormTradeList (map f tl)
