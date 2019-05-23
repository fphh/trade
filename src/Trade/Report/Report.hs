{-# LANGUAGE FlexibleContexts #-}


module Trade.Report.Report where

import Control.Monad.Reader (runReaderT)

import Data.ByteString.Lazy (ByteString)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Time.Clock

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html.Renderer.Utf8 as HtmlBSL
import qualified Text.Blaze.Html5.Attributes as H5A

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Report.Config (HtmlReader, defConfig)

import Trade.Report.Style (AxisConfig(..), colors, impulseAxisConf, axTitle)

import Trade.Report.Line (Line, line)

import qualified Trade.Report.Render as Render

import Trade.Report.ToReport (toReport)

import Trade.Strategy.Algorithm (alignedSignals2signals)
import Trade.Strategy.Type (AlignedSignals(..))

import Trade.Type.Equity (Equity)
import Trade.Type.Impulse (Impulse)
import Trade.Type.ImpulseSignal (ImpulseSignal, curve)
import Trade.Type.Signal (Signal)

import Debug.Trace


clear :: H5.Attribute
clear = H5A.style (H5.stringValue "clear:both;")

header :: String -> HtmlReader ()
header = toReport . (H5.h1 ! clear) . H5.toHtml

subheader :: String -> HtmlReader ()
subheader = toReport . (H5.h2 ! clear) . H5.toHtml

subsubheader :: String -> HtmlReader ()
subsubheader = toReport . (H5.h3 ! clear) . H5.toHtml

text :: String -> HtmlReader ()
text = toReport . H5.p . H5.toHtml

renderReport :: HtmlReader () -> IO ByteString
renderReport html = do
  config <- defConfig
  let sty = H5A.style (H5.stringValue "font-family:monospace;padding:20px;")
      bodySty = H5A.style (H5.stringValue "width:20000px")

      innerHtml = runReaderT html config
      
      doc = do
        H5.docType
        H5.html ! sty $ do
          (H5.body ! bodySty) $ innerHtml
          
  return (HtmlBSL.renderHtml doc)



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



chart ::
  (Foldable t, E.ToPlot p, E.PlotValue x, E.PlotValue y) =>
  AxisConfig x b -> (c, t (E.EC (E.Layout x y) (p x y))) -> HtmlReader ()
chart acx (acy, ls) =
  let diagram = do
        E.setColors colors
        
        E.layout_x_axis E..= xAxisLayout acx
        E.layout_bottom_axis_visibility E..= axisVisibility acx
        case axisFn acx of
          Just x -> E.layout_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

{-
        E.layout_y_axis E..= xAxisLayout acy
        E.layout_left_axis_visibility E..= axisVisibility acy
        case axisFn acy of
          Just x -> E.layout_y_axis . E.laxis_generate E..= x
          Nothing -> return ()
-}

        mapM_ E.plot ls

  in Render.ec2svg diagram


gridChart ::
  (E.PlotValue x, E.PlotValue b, Foldable t, E.ToPlot p) =>
  AxisConfig x b -> t (E.EC (E.Layout x b) (p x b)) -> E.StackedLayout x
gridChart  ac ls =
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


impulseSignalCharts ::
  (Ord x, E.PlotValue x) =>
  [Vector (x, Maybe Impulse)] -> [E.StackedLayout x]
impulseSignalCharts is =
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


backtestChart :: (Ord x) => E.StackedLayout x -> [E.StackedLayout x] -> HtmlReader ()
backtestChart a as =
  let layouts = E.StackedLayouts (a : as) False
  in Render.renderable2svg (E.renderStackedLayouts layouts)


strategyChart ::
  ( Show sym
  , Eq ohlc
  , E.PlotValue ohlc
  , Ord t
  , E.PlotValue t
  , Line (Vector (t, ohlc))) =>
  Map p (ImpulseSignal stgy t) -> AlignedSignals sym t ohlc -> Maybe (Signal t Equity) -> HtmlReader ()
strategyChart is asigs@(AlignedSignals ts _) output = do
  let f sym (Just ss) acc = line (show sym) ss : acc
      f _ _ acc = acc

      mout = maybe [] ((:[]) . line "Equity out") output
      cs = Map.foldrWithKey' f mout (alignedSignals2signals asigs)

      g _ i acc = curve ts i : acc
      ks = Map.foldrWithKey' g [] is
  
  backtestChart
    (gridChart (axTitle "Time" "Equity / Price") cs)
    (impulseSignalCharts ks)

