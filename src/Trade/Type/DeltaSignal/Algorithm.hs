{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.DeltaSignal.Algorithm where

import Data.Time.Clock (diffUTCTime)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as Vec

import Trade.Type.Delta (ToDelta, toDelta, Delta(..))
import qualified Trade.Type.Delta as Delta
import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Equity (Equity)
import Trade.Type.NestedMap (NestedMap(..))
import Trade.Type.Position (Position(..))
import Trade.Type.WinningLosing (WinningLosing(..))

import Trade.Type.Signal (Timeseries, Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Yield (LogYield(..))

import Trade.Type.Step.Algorithm (stepFunction, StepFunction)

import Trade.Help.SafeTail (shead)


deltaSignal ::
  (ToDelta ohlc) =>
  (ohlc -> ohlc -> Delta ohlc) -> Timeseries ohlc -> DeltaSignal ohlc
deltaSignal g (Signal as) =
  let (t0, y0) = shead "deltaSignal: empty signal" as
      f (t, y) = (t `diffUTCTime` t0, g y0 y)
  in DeltaSignal t0 Invested (Signal (Vec.map f (Vec.tail as)))
  -- in DeltaSignal t0 Invested (Signal (Vec.map f as))

longDeltaSignal :: (ToDelta ohlc) => Timeseries ohlc -> DeltaSignal ohlc
longDeltaSignal = deltaSignal toDelta

shortDeltaSignal :: (ToDelta ohlc) => Timeseries ohlc -> DeltaSignal ohlc
shortDeltaSignal = deltaSignal (\a b -> Delta.negate (toDelta a b))


concatDeltaSignals ::
  (StepFunction step) =>
  step -> Equity -> DeltaTradeList ohlc -> Timeseries Equity
concatDeltaSignals _ _ (DeltaTradeList []) = mempty
concatDeltaSignals step a (DeltaTradeList (d:ds)) =
  let f sig es = sig <> stepFunction step (snd (Signal.last sig)) es
  in List.foldl' f (stepFunction step a d) ds

yield :: DeltaSignal ohlc -> LogYield ohlc
yield (DeltaSignal _ _ ds) =
  let (tn, Delta yn) = Signal.last ds
  in LogYield tn (log (1+yn)) 

maximum :: DeltaSignal ohlc -> LogYield ohlc
maximum (DeltaSignal _ _ ds) =
  let (t, Delta y) = Signal.maximum ds
  in LogYield t (log (1+y))

minimum :: DeltaSignal ohlc -> LogYield ohlc
minimum (DeltaSignal _ _ ds) =
  let (t, Delta y) = Signal.minimum ds
  in LogYield t (log (1+y))

sortDeltaSignals ::
  DeltaTradeList ohlc -> NestedMap Position WinningLosing [DeltaSignal ohlc]
sortDeltaSignals (DeltaTradeList dtl) =
  let sortByPosition =
        let f ds = Map.insertWith (++) (position ds) [ds] 
        in List.foldr f Map.empty

      sortByWinnerOrLoser =
        let f wl = Map.insertWith (++) (if logYield (yield wl) <= 0 then Losing else Winning) [wl]
        in List.foldr f Map.empty

  in NestedMap (fmap sortByWinnerOrLoser (sortByPosition dtl))

