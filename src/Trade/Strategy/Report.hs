{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Trade.Strategy.Report where


import qualified Data.Map as Map
import Data.Map (Map)

import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import Trade.Type.ImpulseSignal (ImpulseSignal, curve)

import Trade.Strategy.Algorithm (alignedSignals2signals)
import Trade.Strategy.Type (AlignedSignals(..))

import Trade.Report.Line (Line(..), XTy, YTy, ToLine)
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Style as Style
import Trade.Report.HtmlIO (HtmlIO)

import Trade.Strategy.Type
import Data.Vector

{-
plot ::
  forall stgy sym t ohlc.
  (Show sym, Eq t, Ord t, PlotValue t, Line.TyY (Vector (t, ohlc)) ~ Double) =>
  Map sym (ImpulseSignal stgy t) -> AlignedSignals sym t ohlc -> HtmlIO
  -}
plot ::
  ( Show sym
  , Eq ohlc
  , PlotValue ohlc
  , Ord t
  , PlotValue t
  , ToLine (Vector (t, ohlc))
  , XTy (Vector (t, ohlc)) ~ t
  , YTy (Vector (t, ohlc)) ~ ohlc) =>
  Map p (ImpulseSignal stgy t) -> AlignedSignals sym t ohlc -> HtmlIO
plot is asigs@(AlignedSignals ts _) = do
  let -- f :: _
      f sym (Just ss) acc = Line (show sym) ss : acc
      f _ _ acc = acc

      -- cs :: [Line.L [(t, ohlc)]]
      cs = Map.foldrWithKey' f [] (alignedSignals2signals asigs)

      g _ i acc = curve ts i : acc
      ks = Map.foldrWithKey' g [] is
  
  Rep.backtestChart
    (Rep.gridChart (Style.axTitle "Equity" "Time") cs)
    (Rep.impulseSignalCharts ks)
