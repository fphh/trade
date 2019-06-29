{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Report.Chart where

import Prelude hiding (lines)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Time.Clock (UTCTime)

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Report.Config (HtmlReader)

import Trade.Report.Style (AxisConfig(..), colors, impulseAxisConf, axTitle)

import Trade.Report.Line (Line, line)

import qualified Trade.Report.Render as Render

import Trade.Strategy.Algorithm (alignedSignals2signals)
import Trade.Strategy.Type (AlignedSignals(..))

import Trade.Type.Equity (Equity)
import Trade.Type.Impulse (Impulse)
import Trade.Type.ImpulseSignal (ImpulseSignal(..), curve)
import Trade.Type.Signal (Timeseries, Signal(..))
import qualified Trade.Type.Signal as Signal



candle :: String -> [Vector (E.Candle UTCTime Double)] -> HtmlReader ()
candle lbl cs =
  let lineStyle n colour =
        E.line_width E..~ n
        $ E.line_color E..~ E.opaque colour
        $ E.def

      diagram :: E.EC (E.Layout UTCTime Double) (E.PlotCandle UTCTime Double)
      diagram = E.liftEC $ do
        E.plot_candle_line_style  E..= lineStyle 1 E.darkblue
        E.plot_candle_fill E..= True
        E.plot_candle_rise_fill_style E..= E.solidFillStyle (E.opaque E.white)
        E.plot_candle_fall_fill_style E..= E.solidFillStyle (E.opaque E.red)
        E.plot_candle_tick_length E..= 0
        E.plot_candle_width E..= 2

        mapM_ ((E.plot_candle_values E..=) . Vec.toList) cs

        E.plot_candle_title E..= lbl
    
  in Render.ec2svg (E.plot diagram)



lines ::
  (Foldable t, E.ToPlot p, E.PlotValue x, E.PlotValue y) =>
  AxisConfig x b -> t (E.EC (E.Layout x y) (p x y)) -> HtmlReader ()
lines acx ls =
  let diagram = do
        E.setColors colors
        
        E.layout_x_axis E..= xAxisLayout acx
        E.layout_bottom_axis_visibility E..= axisVisibility acx
        case axisFn acx of
          Just x -> E.layout_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        mapM_ E.plot ls

  in Render.ec2svg diagram


grid ::
  (E.PlotValue x, E.PlotValue b, Foldable t, E.ToPlot p) =>
  AxisConfig x b -> t (E.EC (E.Layout x b) (p x b)) -> E.StackedLayout x
grid  ac ls =
  let diagram = E.execEC $ do
        E.setColors colors
 
        E.layout_x_axis E..= xAxisLayout ac
        E.layout_y_axis E..= yAxisLayout ac
        E.layout_bottom_axis_visibility E..= axisVisibility ac
        
        case axisFn ac of
          Just x -> E.layout_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        mapM_ E.plot ls

  in E.StackedLayout diagram


impulseSignals ::
  (Ord x, E.PlotValue x) =>
  [Vector (x, Maybe Impulse)] -> [E.StackedLayout x]
impulseSignals is =
  let toChart sty ls = E.execEC $ do
        E.setColors [ E.opaque E.darkgreen ]
        
        E.layout_x_axis E..= xAxisLayout sty
        E.layout_legend E..= Nothing
        E.layout_bottom_axis_visibility E..= (E.axis_show_labels E..~ False $ E.def)
        
        E.layout_y_axis E..= yAxisLayout sty
        E.layout_left_axis_visibility E..= axisVisibility sty
        
        case axisFn sty of
          Just x -> E.layout_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        mapM_ E.plot ls
        
      f _impSig = toChart impulseAxisConf [E.line "down buy / up sell" (map Vec.toList is)]
  in map (E.StackedLayout . f) is


backtest :: (Ord x) => E.StackedLayout x -> [E.StackedLayout x] -> HtmlReader ()
backtest a as =
  let layouts = E.StackedLayouts (a : as) False
  in Render.renderable2svg (E.renderStackedLayouts layouts)


strategy ::
  ( Show sym
  , Eq ohlc
  , E.PlotValue ohlc
  , Line (Vector (UTCTime, ohlc))) =>
  Map p (ImpulseSignal stgy) -> AlignedSignals sym ohlc -> Map sym (Timeseries Equity) -> HtmlReader ()
strategy is asigs@(AlignedSignals ts _) output = do
  let h sym o acc = line (show sym) o : acc
      mout = Map.foldrWithKey' h [] output

      f sym (Just ss) acc = line (show sym) ss : acc
      f _ _ acc = acc
      cs = Map.foldrWithKey' f mout (alignedSignals2signals asigs)

      g _ i acc = curve ts i : acc
      ks = Map.foldrWithKey' g [] is
  
  backtest
    (grid (axTitle "Time" "Equity / Price") cs)
    (impulseSignals ks)

bt :: Map UTCTime Impulse -> Timeseries Equity -> HtmlReader ()
bt m sig@(Signal as) =
  let (t0, _) = Vec.head as
      (tn, _) = Vec.last as
      is = (t0, Nothing) `Vec.cons` (Vec.map (fmap Just) (Vec.fromList (Map.toList m))) `Vec.snoc` (tn, Nothing)
  in backtest
     (grid (axTitle "Time" "Equity / Price") [line "Equity" sig])
     (impulseSignals [is])

  

input ::
  forall sym t x.
  ( Show sym
  , Line (Signal t x)
  , E.PlotValue t
  , E.PlotValue x) =>
  Map sym (Signal t x) -> HtmlReader ()
input m =
  let h sym o acc = line (show sym) o : acc
      mout = Map.foldrWithKey' h [] m
      title :: AxisConfig t x
      title = axTitle "Time" "Price"
  in lines title mout

