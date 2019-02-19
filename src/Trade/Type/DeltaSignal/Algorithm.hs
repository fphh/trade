{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.DeltaSignal.Algorithm where

import qualified Data.List as List
import qualified Data.Vector as Vec

import Trade.Type.Bars (Add, diff)
import Trade.Type.Delta (ToDelta, toDelta, Delta)
import qualified Trade.Type.Delta as Delta
import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.DeltaTradeList (DeltaTradeList(..))
import Trade.Type.Equity (Equity)
import Trade.Type.Position (Position(..))
import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Step.Algorithm (stepFunction, StepFunction)

import Trade.Help.SafeTail (shead)


deltaSignal ::
  (ToDelta ohlc, Add t) =>
  (ohlc -> ohlc -> Delta ohlc) -> Signal t ohlc -> DeltaSignal t ohlc
deltaSignal g (Signal as) =
  let (t0, y0) = shead "toDeltaSignal: empty signal" as
      f (t, y) = (t `diff` t0, g y0 y)
  in DeltaSignal t0 Invested (Signal (Vec.map f as))

constDeltaSignal :: (ToDelta ohlc, Add t) => Signal t ohlc -> DeltaSignal t ohlc
constDeltaSignal sig = (deltaSignal (\_ _ -> Delta.zero) sig) { position = NotInvested }

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

rebase :: t -> DeltaTradeList t ohlc -> DeltaTradeList t ohlc
rebase _ (DeltaTradeList []) = DeltaTradeList []
rebase t (DeltaTradeList es@(d:ds)) =
  let f a b = b { start = t }
  in DeltaTradeList ((d { start = t }) : zipWith f es ds)
