

module Trade.Type.Conversion.Impulse2Trade where

import qualified Data.Vector as Vec

import Data.Maybe (isNothing)


import Trade.Type.Bars (Time)

import Trade.Timeseries.Algorithm.SyncZip

import Trade.Type.Position (Position(..))
import Trade.Type.Impulse (Impulse(..))
import Trade.Type.Signal (Signal(..))
import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Type.Trade (Trade(..), TradeList(..))

import Trade.Help.SafeTail

import Debug.Trace



impulse2trade :: (Ord t) => Signal t ohlc -> ImpulseSignal t -> TradeList t ohlc
impulse2trade (Signal ps) (Signal is) =
  let ss = syncZip ps is
      impulse (_, (_, x)) = x

      go vs | Vec.null vs = []
      go vs =
        let (a, as) = (shead "impulses2trades (1)" vs, stail "impulses2trades" vs)

            (xs, ys) = Vec.span (isNothing . impulse) as
        in Vec.cons a xs : go ys

      us = go (Vec.dropWhile (isNothing . impulse) ss)

      f as bs = Vec.snoc as (shead "impulses2trades (2)" bs)
      ns = zipWith f us (tail us) -- ++ [last us]

      g vs =
        flip Trade (Vec.map (\(a, (b, _)) -> (a, b)) vs)
        $ case Vec.head vs of
            (_, (_, Just Buy)) -> Long
            _ -> NoPosition
          
  in TradeList (map g ns)

