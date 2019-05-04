

module Trade.Strategy.Report where


import qualified Data.Map as Map
import Data.Map (Map)

import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import Trade.Type.ImpulseSignal (ImpulseSignal, curve)

import Trade.Strategy.Algorithm (alignedSignals2signals)
import Trade.Strategy.Type (AlignedSignals(..))

import qualified Trade.Report.Line as Line
import qualified Trade.Report.Report as Rep
import qualified Trade.Report.Style as Style
import Trade.Report.HtmlIO (HtmlIO)


plot ::
  (Show sym, Eq t, Ord t, PlotValue t) =>
  Map sym (ImpulseSignal stgy t) -> AlignedSignals sym t Double -> HtmlIO
plot is asigs@(AlignedSignals ts _) = do
  let f sym (Just ss) acc = Line.line (show sym) ss : acc
      f _ _ acc = acc
      
      cs = Map.foldrWithKey' f [] (alignedSignals2signals asigs)

      g sym i acc = curve ts i : acc
      ks = Map.foldrWithKey' g [] is
  
  Rep.backtestChart
    (Rep.gridChart (Style.axTitle "Equity") cs)
    (Rep.impulseSignalCharts ks)
