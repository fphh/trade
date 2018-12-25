

module Trade.Type.Conversion.Impulse2TradeList where

import qualified Data.Vector as Vec 

import qualified Data.Map as Map

import Trade.Type.Position (Position(NoPosition))
import Trade.Type.Strategy (Strategy(..))

import Trade.Type.Signal (Signal(..))
import Trade.Type.ImpulseSignal (ImpulseSignal(..))
import Trade.Type.Trade (Trade(..), TradeList(..))

import Trade.Type.Conversion.Impulse2Position (impulse2position)


impulse2tradeList :: (Ord t) => Strategy -> Signal t ohlc -> ImpulseSignal t -> TradeList t ohlc
impulse2tradeList stgy (Signal ps) (ImpulseSignal is) =
  let len = Vec.length ps - 1

      h i (t, _) acc = maybe acc (\bs -> ((bs, i):acc)) (Map.lookup t is)      
      ss = Vec.ifoldr' h [] ps

      (_, fidx) = head ss
      firstTrade =
        case fidx > 0 of
          True -> ([Trade NoPosition (Vec.slice 0 (fidx+1) ps)] ++)
          False -> ([] ++)

      (limp, lidx) = last ss
      lastTrade =
        case lidx < len of
          True -> (++ [Trade (impulse2position stgy limp) (Vec.slice lidx (len-lidx+1) ps)])
          False -> (++ [])

      f (c, i) (_, j) = Trade (impulse2position stgy c) (Vec.slice i (j-i+1) ps)
      trds = firstTrade (lastTrade (zipWith f ss (tail ss)))
      
  in case Map.null is of
       True -> TradeList [Trade NoPosition ps]
       False -> TradeList trds


{-

Trade {tradePosition = ShortPosition, ticker = [

          (2017-01-01 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 10.5}, _ohlcHigh = High {unHigh = 11.0}, _ohlcLow = Low {unLow = 9.0}, _ohlcClose = Close {unClose = 10.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),
          (2017-01-02 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 11.5}, _ohlcHigh = High {unHigh = 12.0}, _ohlcLow = Low {unLow = 10.0}, _ohlcClose = Close {unClose = 11.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),
          (2017-01-03 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 12.5}, _ohlcHigh = High {unHigh = 13.0}, _ohlcLow = Low {unLow = 11.0}, _ohlcClose = Close {unClose = 12.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),
          (2017-01-04 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 13.5}, _ohlcHigh = High {unHigh = 14.0}, _ohlcLow = Low {unLow = 12.0}, _ohlcClose = Close {unClose = 13.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),
          (2017-01-05 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 14.5}, _ohlcHigh = High {unHigh = 15.0}, _ohlcLow = Low {unLow = 13.0}, _ohlcClose = Close {unClose = 14.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),
          (2017-01-06 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 15.5}, _ohlcHigh = High {unHigh = 16.0}, _ohlcLow = Low {unLow = 14.0}, _ohlcClose = Close {unClose = 15.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),
          (2017-01-07 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 16.5}, _ohlcHigh = High {unHigh = 17.0}, _ohlcLow = Low {unLow = 15.0}, _ohlcClose = Close {unClose = 16.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),
          (2017-01-08 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 17.5}, _ohlcHigh = High {unHigh = 18.0}, _ohlcLow = Low {unLow = 16.0}, _ohlcClose = Close {unClose = 17.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),
          (2017-01-09 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 18.5}, _ohlcHigh = High {unHigh = 19.0}, _ohlcLow = Low {unLow = 17.0}, _ohlcClose = Close {unClose = 18.0}, _ohlcVolume = Volume {unVolume = 1000.0}})]},



Trade {tradePosition = NoPosition, ticker = [
          (2017-01-09 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 18.5}, _ohlcHigh = High {unHigh = 19.0}, _ohlcLow = Low {unLow = 17.0}, _ohlcClose = Close {unClose = 18.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),
          (2017-01-10 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 17.5}, _ohlcHigh = High {unHigh = 18.0}, _ohlcLow = Low {unLow = 16.0}, _ohlcClose = Close {unClose = 17.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-11 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 16.5}, _ohlcHigh = High {unHigh = 17.0}, _ohlcLow = Low {unLow = 15.0}, _ohlcClose = Close {unClose = 16.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-12 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 15.5}, _ohlcHigh = High {unHigh = 16.0}, _ohlcLow = Low {unLow = 14.0}, _ohlcClose = Close {unClose = 15.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-13 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 14.5}, _ohlcHigh = High {unHigh = 15.0}, _ohlcLow = Low {unLow = 13.0}, _ohlcClose = Close {unClose = 14.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-14 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 13.5}, _ohlcHigh = High {unHigh = 14.0}, _ohlcLow = Low {unLow = 12.0}, _ohlcClose = Close {unClose = 13.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-15 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 12.5}, _ohlcHigh = High {unHigh = 13.0}, _ohlcLow = Low {unLow = 11.0}, _ohlcClose = Close {unClose = 12.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-16 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 11.5}, _ohlcHigh = High {unHigh = 12.0}, _ohlcLow = Low {unLow = 10.0}, _ohlcClose = Close {unClose = 11.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-17 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 10.5}, _ohlcHigh = High {unHigh = 11.0}, _ohlcLow = Low {unLow = 9.0}, _ohlcClose = Close {unClose = 10.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-18 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 9.5}, _ohlcHigh = High {unHigh = 10.0}, _ohlcLow = Low {unLow = 8.0}, _ohlcClose = Close {unClose = 9.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-19 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 8.5}, _ohlcHigh = High {unHigh = 9.0}, _ohlcLow = Low {unLow = 7.0}, _ohlcClose = Close {unClose = 8.0}, _ohlcVolume = Volume {unVolume = 1000.0}}),(2017-01-20 00:00:00 UTC,OHLC {_ohlcOpen = Open {unOpen = 7.5}, _ohlcHigh = High {unHigh = 8.0}, _ohlcLow = Low {unLow = 6.0}, _ohlcClose = test.txt

-}
