{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.DeltaSignal.Algorithm where

import qualified Data.List as List
import qualified Data.Vector as Vec

import Trade.Type.Bars (DeltaTy, Add, diff)
import Trade.Type.Delta (ToDelta, toDelta, Delta(..))
import qualified Trade.Type.Delta as Delta
import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Equity (Equity)
import Trade.Type.Position (Position(..))

import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Yield (LogYield(..))

import Trade.Type.Step.Algorithm (stepFunction, StepFunction)

import Trade.Help.SafeTail (shead)


deltaSignal ::
  (ToDelta ohlc, Add t) =>
  (ohlc -> ohlc -> Delta ohlc) -> Signal t ohlc -> DeltaSignal t ohlc
deltaSignal g (Signal as) =
  let (t0, y0) = shead "deltaSignal: empty signal" as
      f (t, y) = (t `diff` t0, g y0 y)
  in DeltaSignal t0 Invested (Signal (Vec.map f as))

longDeltaSignal :: (ToDelta ohlc, Add t) => Signal t ohlc -> DeltaSignal t ohlc
longDeltaSignal = deltaSignal toDelta

shortDeltaSignal :: (ToDelta ohlc, Add t) => Signal t ohlc -> DeltaSignal t ohlc
shortDeltaSignal = deltaSignal (\a b -> Delta.negate (toDelta a b))


concatDeltaSignals ::
  (Add t, StepFunction step t) => step t -> Equity -> DeltaTradeList t ohlc -> Signal t Equity
concatDeltaSignals _ _ (DeltaTradeList []) = mempty
concatDeltaSignals step a (DeltaTradeList (d:ds)) =
  let f sig es = sig <> stepFunction step (snd (Signal.last sig)) es
  in List.foldl' f (stepFunction step a d) ds


yield :: DeltaSignal t ohlc -> LogYield (DeltaTy t) ohlc
yield (DeltaSignal _ _ ds) =
  let (tn, Delta yn) = Signal.last ds
  in LogYield tn (log (1+yn)) 

maximum :: (Eq (DeltaTy t)) => DeltaSignal t ohlc -> LogYield (DeltaTy t) ohlc
maximum (DeltaSignal _ _ ds) =
  let (t, Delta y) = Signal.maximum ds
  in LogYield t (log (1+y))

minimum :: (Eq (DeltaTy t)) => DeltaSignal t ohlc -> LogYield (DeltaTy t) ohlc
minimum (DeltaSignal _ _ ds) =
  let (t, Delta y) = Signal.minimum ds
  in LogYield t (log (1+y))
