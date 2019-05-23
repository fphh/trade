{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Trade.Strategy.Report where

import Data.Vector (Vector)

import qualified Data.Map as Map
import Data.Map (Map)

import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import Trade.Type.Equity (Equity)
import Trade.Type.ImpulseSignal (ImpulseSignal, curve)
import Trade.Type.Signal (Signal)

import Trade.Strategy.Algorithm (alignedSignals2signals)
import Trade.Strategy.Type (AlignedSignals(..))

import Trade.Report.Line (Line, line)
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Style as Style
import Trade.Report.Config (HtmlReader)


plot ::
  ( Show sym
  , Eq ohlc
  , PlotValue ohlc
  , Ord t
  , PlotValue t
  , Line (Vector (t, ohlc))) =>
  Map p (ImpulseSignal stgy t) -> AlignedSignals sym t ohlc -> Maybe (Signal t Equity) -> HtmlReader ()
plot is asigs@(AlignedSignals ts _) output = do
  let f sym (Just ss) acc = line (show sym) ss : acc
      f _ _ acc = acc

      mout = maybe [] ((:[]) . line "Equity out") output
      cs = Map.foldrWithKey' f mout (alignedSignals2signals asigs)

      g _ i acc = curve ts i : acc
      ks = Map.foldrWithKey' g [] is
  
  Rep.backtestChart
    (Rep.gridChart (Style.axTitle "Time" "Equity / Price") cs)
    (Rep.impulseSignalCharts ks)

