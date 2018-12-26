{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.DeltaSignal where


import qualified Data.Vector as Vec

import Trade.Type.Delta (Delta(..), DeltaTy, DDelta, CDelta, Add, add, diff)
import Trade.Type.Scale (Scale, scale, factor)

import Trade.Type.Equity (Equity(..))

import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal


import Trade.Help.SafeTail (shead)

data DeltaSignal t ohlc = DeltaSignal {
  basis :: (t, ohlc)
  , delta :: Signal (DeltaTy t) (DeltaTy ohlc)
  }

deriving instance (Show t, Show ohlc, Show (DeltaTy t), Show (DeltaTy ohlc)) => Show (DeltaSignal t ohlc)

scaleDeltaSignal ::
  (Scale ohlc, Scale (DeltaTy ohlc)) =>
  Double -> DeltaSignal t ohlc -> DeltaSignal t ohlc
scaleDeltaSignal q (DeltaSignal x (Signal dxs)) =
  let f (t, dx) = (t, scale q dx)
  in DeltaSignal (fmap (scale q) x) (Signal (Vec.map f dxs))

factorDeltaSignal :: (Scale ohlc) => Double -> DeltaSignal t ohlc -> Double
factorDeltaSignal q (DeltaSignal (_, b) _) = factor q b

constDeltaSignal :: (DeltaTy ohlc ~ CDelta ohlc) => DeltaSignal t ohlc -> DeltaSignal t ohlc
constDeltaSignal (DeltaSignal b (Signal dxs)) =
  let f = const (Delta 0.0)
  in DeltaSignal b (Signal (Vec.map (fmap f) dxs))

shortDeltaSignal ::
  (Scale ohlc, Scale (DeltaTy ohlc)) => DeltaSignal t ohlc -> DeltaSignal t ohlc
shortDeltaSignal (DeltaSignal x (Signal dxs)) =
  let f (t, dx) = (t, scale (-1) dx)
  in DeltaSignal x (Signal (Vec.map f dxs))

-- | TODO: Check, wether `toDeltaSignal . fromDeltaSignal = id`.
toDeltaSignal ::
  (Add t, Add ohlc) => Signal t ohlc -> DeltaSignal t ohlc
toDeltaSignal (Signal as) =
  let a = shead "toDeltaSignal" as
  in DeltaSignal a (Signal (Vec.map (flip diff a) as))

fromDeltaSignal ::
  (Add t, Add ohlc) => DeltaSignal t ohlc -> Signal t ohlc
fromDeltaSignal (DeltaSignal a (Signal as)) =
  Signal (Vec.map (flip add a) as)


rebaseDeltaSignal :: (t, ohlc) -> DeltaSignal t ohlc -> DeltaSignal t ohlc
rebaseDeltaSignal b (DeltaSignal _ xs) = DeltaSignal b xs

concatDeltaSignal ::
  (Add t, Add ohlc, Scale ohlc, Scale (DeltaTy ohlc)) =>
  Equity -> DeltaSignal t ohlc -> DeltaSignal t ohlc -> Signal t ohlc -- Equity
concatDeltaSignal (Equity eqty) d0@(DeltaSignal (t0, y0) dxs0) d1@(DeltaSignal (t1, y1) dxs1) =
  let f = factorDeltaSignal eqty d0
      s = scaleDeltaSignal f d0
      sig0 = fromDeltaSignal s
      l0 = Signal.last sig0
      newD1 = rebaseDeltaSignal l0 d1
      sig1 = fromDeltaSignal newD1
  in sig0 <> sig1
