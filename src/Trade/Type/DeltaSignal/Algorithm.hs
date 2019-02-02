{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.DeltaSignal.Algorithm where

import qualified Data.List as List
import qualified Data.Vector as Vec

import Trade.Type.Bars (Add, diff)

import Trade.Type.Delta (ToDelta, toDelta)
import qualified Trade.Type.Delta as Delta

import Trade.Type.DeltaSignal (DeltaSignal(..))

import Trade.Type.Equity (Equity)
import Trade.Type.Position (Position(..))

import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Step.Algorithm (stepFunction, StepFunction)

import Trade.Help.SafeTail (shead)


constDeltaSignal :: DeltaSignal t ohlc -> DeltaSignal t ohlc
constDeltaSignal (DeltaSignal t _ (Signal dxs)) =
  let f = const Delta.zero
  in DeltaSignal t NotInvested (Signal (Vec.map (fmap f) dxs))

shortDeltaSignal :: DeltaSignal t ohlc -> DeltaSignal t ohlc
shortDeltaSignal (DeltaSignal t0 _ (Signal dxs)) =
  DeltaSignal t0 Invested (Signal (Vec.map (fmap Delta.negate) dxs))

-- | TODO: Check, wether `toDeltaSignal . fromDeltaSignal y0 = id`.
toDeltaSignal :: (ToDelta ohlc, Add t) => Signal t ohlc -> DeltaSignal t ohlc
toDeltaSignal (Signal as) =
  let (t0, y0) = shead "toDeltaSignal: empty signal" as
      f (t, y) = (t `diff` t0, toDelta y0 y)
  in DeltaSignal t0 Invested (Signal (Vec.map f as))

concatDeltaSignals ::
  (Add t, StepFunction step t) => step t -> Equity -> [DeltaSignal t ohlc] -> Signal t Equity
concatDeltaSignals _ _ [] = mempty
concatDeltaSignals step a (d:ds) =
  let f sig es = sig <> stepFunction step (snd (Signal.last sig)) es
  in List.foldl' f (stepFunction step a d) ds
