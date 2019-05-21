{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Report.Report where

import Control.Monad.Reader (runReaderT)

import Data.ByteString.Lazy (ByteString)


import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Time.Clock

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html.Renderer.Utf8 as HtmlBSL
import qualified Text.Blaze.Html5.Attributes as H5A

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Report.Config (HtmlReader, defConfig)

import Trade.Report.Style (AxisConfig(..), colors, impulseAxisConf)
import Trade.Report.Line (Line(..), ToLine, toLine, XTy, YTy)
import qualified Trade.Report.Render as Render

import Trade.Report.ToReport (toReport)

import Trade.Type.Impulse (Impulse)




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
  (Foldable t, ToLine a, E.PlotValue (XTy a), E.PlotValue (YTy a)) =>
  AxisConfig (XTy a) b -> (AxisConfig (YTy a) c, t (Line a)) -> HtmlReader ()
chart acx (acy, ls) =
  let diagram = do
        E.setColors colors
        
        E.layout_x_axis E..= xAxisLayout acx
        E.layout_bottom_axis_visibility E..= axisVisibility acx
        case axisFn acx of
          Just x -> E.layout_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        E.layout_y_axis E..= xAxisLayout acy
        E.layout_left_axis_visibility E..= axisVisibility acy
        case axisFn acy of
          Just x -> E.layout_y_axis . E.laxis_generate E..= x
          Nothing -> return ()
          
        mapM_ (E.plot . toLine) ls
        
  in Render.ec2svg diagram


gridChart ::
  (Ord (YTy a), E.PlotValue (XTy a), E.PlotValue (YTy a), Foldable t, ToLine a) =>
  AxisConfig (XTy a) (YTy a) -> t (Line a) -> E.StackedLayout (XTy a)
gridChart  ac ls =
  let diagram = E.execEC $ do
        E.setColors colors
 
        E.layout_x_axis E..= xAxisLayout ac
        E.layout_y_axis E..= yAxisLayout ac
        E.layout_bottom_axis_visibility E..= axisVisibility ac
        
        case axisFn ac of
          Just x -> E.layout_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        mapM_ (E.plot . toLine) ls

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

  
{-
divs :: [Html] -> Reader a Html
divs ps =
  let sty = H5A.style (H5.stringValue "margin-bottom:16px;")
      f = liftHtml (H5.div ! sty)
  in return (mapM_ f ps)
-}

{-

clear :: H5.Attribute
clear = H5A.style (H5.stringValue "clear:both;")

header :: String -> HtmlIO
header = HtmlT . return . (H5.h1 ! clear) . H5.toHtml

subheader :: String -> HtmlIO
subheader = HtmlT . return . (H5.h2 ! clear) . H5.toHtml

subsubheader :: String -> HtmlIO
subsubheader = HtmlT . return . (H5.h3 ! clear) . H5.toHtml

text :: String -> HtmlIO
text = HtmlT . return . H5.p . H5.toHtml

{-
htable :: [[String]] -> HtmlIO
htable ss = (HtmlT . return) $
  let sty = H5A.style (H5.stringValue "min-width:200px;text-align:left;")
  in H5.table $ do
       H5.thead $ do
         H5.tr $ mapM_ ((H5.th ! sty) . H5.toHtml) (head ss)
       H5.tbody $ do
         mapM_ (H5.tr . mapM_ (H5.td . H5.toHtml)) (tail ss)
  
vtable :: [[String]] -> HtmlIO
vtable ss = (HtmlT . return) $
  let sty = H5A.style (H5.stringValue "min-width:200px;text-align:left;")
  in H5.table $ do
       H5.thead $ do
         return ()
       H5.tbody $ do
         mapM_ (H5.tr . mapM_ ((H5.td ! sty) . H5.toHtml)) ss
-}

divs :: [HtmlIO] -> HtmlIO
divs ps =
  let sty = H5A.style (H5.stringValue "margin-bottom:16px;")
      f = liftHtml (H5.div ! sty)
  in mapM_ f ps


horizontal :: HtmlIO -> HtmlIO
horizontal = liftHtml $ \x -> do
  let sty = H5A.style (H5.stringValue "clear:both")
  H5.div x
  H5.div ! sty $ mempty


floatLeft :: HtmlIO -> HtmlIO
floatLeft = liftHtml $
  let sty = H5A.style (H5.stringValue "float:left;")
  in H5.div ! sty
  

renderReport :: HtmlIO -> IO ByteString
renderReport html = do
  html' <- runHtmlT html
  let sty = H5A.style (H5.stringValue "font-family:monospace;padding:20px;")
      bodySty = H5A.style (H5.stringValue "width:20000px")
      doc = do
        H5.docType
        H5.html ! sty $ do
          (H5.body ! bodySty) html'
  return (HtmlBSL.renderHtml doc)


candle :: String -> [Vector (E.Candle UTCTime Double)] -> HtmlIO
candle label cs = HtmlT $ do
  let lineStyle n colour = E.line_width E..~ n
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

        E.plot_candle_title E..= label
    
  fmap H5.unsafeLazyByteString (Render.ec2svg (E.plot diagram))

chart ::
  (Foldable t, ToLine a, E.PlotValue (XTy a), E.PlotValue (YTy a)) =>
  AxisConfig (XTy a) b -> (AxisConfig (YTy a) c, t (Line a)) -> HtmlT IO ()
chart acx (acy, ls) = HtmlT $ do
  let diagram = do
        E.setColors colors
        
        E.layout_x_axis E..= xAxisLayout acx
        E.layout_bottom_axis_visibility E..= axisVisibility acx
        case axisFn acx of
          Just x -> E.layout_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        E.layout_y_axis E..= xAxisLayout acy
        E.layout_left_axis_visibility E..= axisVisibility acy
        case axisFn acy of
          Just x -> E.layout_y_axis . E.laxis_generate E..= x
          Nothing -> return ()
          
        mapM_ (E.plot . toLine) ls
        
  fmap H5.unsafeLazyByteString (Render.ec2svg diagram)


gridChart ::
  (Ord (YTy a), E.PlotValue (XTy a), E.PlotValue (YTy a), Foldable t, ToLine a) =>
  AxisConfig (XTy a) (YTy a) -> t (Line a) -> E.StackedLayout (XTy a)
gridChart  ac ls =
  let diagram = E.execEC $ do
        E.setColors colors
 
        E.layout_x_axis E..= xAxisLayout ac
        E.layout_y_axis E..= yAxisLayout ac
        E.layout_bottom_axis_visibility E..= axisVisibility ac
        
        case axisFn ac of
          Just x -> E.layout_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        mapM_ (E.plot . toLine) ls

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
      
      f impSig = toChart impulseAxisConf [E.line "down buy / up sell" (map Vec.toList is)]
  in map (E.StackedLayout . f) is

backtestChart :: (Ord x) => E.StackedLayout x -> [E.StackedLayout x] -> HtmlIO
backtestChart a as =  HtmlT $ do
  let layouts = E.StackedLayouts (a : as) False
  fmap H5.unsafeLazyByteString (Render.renderable2svg (E.renderStackedLayouts layouts))
-}






{-
chartLR ::
  AxisConfig x0 b
  -> (AxisConfig y10 b1, [L [(x0, y10)]])
  -> (AxisConfig y20 b2, [L [(x0, y20)]])
  -> HtmlT IO ()
chartLR acx (acL, lsL) (acR, lsR) = HtmlT $ do
  let fstyle = E.def {
        E._font_name = "monospace"
        , E._font_size = 24
        , E._font_weight = E.FontWeightNormal
        }

      df = E.def {
        D._fo_format = D.SVG_EMBEDDED
        , D._fo_fonts = fmap (. (const fstyle)) D.loadCommonFonts
        , D._fo_size = chartSize
        }

  
      diagram = do
        E.setColors colors

        E.layoutlr_x_axis E..= xAxisLayout acx
        E.layoutlr_bottom_axis_visibility E..= axisVisibility acx
        case axisFn acx of
          Just x -> E.layoutlr_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        E.layoutlr_left_axis E..= xAxisLayout acL
        E.layoutlr_left_axis_visibility E..= axisVisibility acL
        case axisFn acL of
          Just x -> E.layoutlr_left_axis . E.laxis_generate E..= x
          Nothing -> return ()

        E.layoutlr_right_axis E..= xAxisLayout acR
        E.layoutlr_right_axis_visibility E..= axisVisibility acR
        case axisFn acR of
          Just x -> E.layoutlr_right_axis . E.laxis_generate E..= x
          Nothing -> return ()

        let toLine (L str vs) = E.line str [vs]
  
        mapM_ E.plotLeft (map toLine lsL)
        mapM_ E.plotRight (map toLine lsR)

  fmap H5.unsafeLazyByteString (toBS df diagram)
-}
