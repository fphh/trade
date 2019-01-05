{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Type.DeltaSignal where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)

import qualified Data.Vector as Vec

import qualified Data.List as List

import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))

import Trade.Type.Signal (Signal(..))
import qualified Trade.Type.Signal as Signal


import Trade.Help.SafeTail (shead)

data Delta ty = Delta Double deriving (Show)


data DeltaSignal = DeltaSignal {
  start :: UTCTime
  , delta :: Signal NominalDiffTime (Delta Price)
  } deriving (Show)

constDeltaSignal :: DeltaSignal -> DeltaSignal
constDeltaSignal (DeltaSignal t (Signal dxs)) =
  let f = const (Delta 0.0)
  in DeltaSignal t (Signal (Vec.map (fmap f) dxs))


shortDeltaSignal ::
  DeltaSignal -> DeltaSignal
shortDeltaSignal (DeltaSignal t0 (Signal dxs)) =
  let f (t, Delta dx) = (t, Delta (negate dx))
  in DeltaSignal t0 (Signal (Vec.map f dxs))


-- | TODO: Check, wether `toDeltaSignal . fromDeltaSignal y0 = id`.
toDeltaSignal ::
  Signal UTCTime Price -> DeltaSignal
toDeltaSignal (Signal as) =
  let (t0, Price y0) = Vec.head as
      f (t, Price y) = (t `diffUTCTime` t0, Delta ((y - y0) / y0))
  in DeltaSignal t0 (Signal (Vec.map f as))



fromDeltaSignal ::
  Equity -> DeltaSignal -> Signal UTCTime Equity
fromDeltaSignal (Equity a) (DeltaSignal t (Signal as)) =
  let f (dt, Delta dy) = (dt `addUTCTime` t, Equity ((dy * a) + a))
  in Signal (Vec.map f as)



concatDeltaSignals ::
  Equity -> [DeltaSignal] -> Signal UTCTime Equity
concatDeltaSignals _ [] = mempty
concatDeltaSignals a (d:ds) =
  let f sig ds = sig <> fromDeltaSignal (snd (Signal.last sig)) ds
  in List.foldl' f (fromDeltaSignal a d) ds


