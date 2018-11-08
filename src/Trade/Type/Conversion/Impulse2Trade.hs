

module Trade.Type.Conversion.Impulse2Trade where

import qualified Data.Vector as Vec 

import qualified Data.Map as Map

import Data.Maybe (isNothing)


import Trade.Type.Bars (Time)

import Trade.Timeseries.Algorithm.SyncZip

import Trade.Type.Position (Position(NoPosition))
-- import Trade.Type.Impulse (Impulse(..))

import Trade.Type.Signal (Signal(..))
import Trade.Type.ImpulseSignal (ImpulseSignal(..))
import Trade.Type.Trade (Trade(..), TradeList(..))

import Trade.Type.Conversion.Impulse2Position (impulse2position)

import Trade.Help.SafeTail

import Debug.Trace


impulse2trade :: (Ord t) => Signal t ohlc -> ImpulseSignal t -> TradeList t ohlc
impulse2trade (Signal ps) (ImpulseSignal is) =
  let len = Vec.length ps - 1

      h i (t, _) acc = maybe acc (\bs -> ((bs, i):acc)) (Map.lookup t is)      
      ss = Vec.ifoldr' h [] ps

      (fimp, fidx) = head ss
      firstTrade =
        case fidx > 0 of
          True -> ([Trade NoPosition (Vec.slice 0 (fidx+1) ps)] ++)
          False -> ([] ++)

      (limp, lidx) = last ss
      lastTrade =
        case lidx < len of
          True -> (++ [Trade (impulse2position limp) (Vec.slice lidx (len-lidx+1) ps)])
          False -> (++ [])

      f (c, i) (_, j) = Trade (impulse2position c) (Vec.slice i (j-i+1) ps)
      trds = firstTrade (lastTrade (zipWith f ss (tail ss)))
      
  in case Map.null is of
       True -> TradeList [Trade NoPosition ps]
       False -> TradeList trds

