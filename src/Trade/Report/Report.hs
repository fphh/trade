{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}


module Trade.Report.Report where

import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)

import Data.ByteString.Lazy (ByteString)

import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Time.Clock


import qualified Graphics.Rendering.Chart.Easy as E
import qualified Graphics.Rendering.Chart.Backend.Diagrams as D

import Trade.Render.Common.Attr
import Trade.Render.Common.Utils

data AxisConfig a = AxisConfig {
  axisLayout :: E.LayoutAxis a
  , axisVisibility :: E.AxisVisibility
--  , axisStyle :: E.AxisStyle
  , axisFn :: Maybe (E.AxisFn a)
  }

axisConfDef ::(E.PlotValue y, RealFloat y, Show y, Num y, E.Default (E.AxisData y)) =>  AxisConfig y
axisConfDef = AxisConfig E.def E.def {- E.def -} Nothing


instance E.Default (E.AxisData Double) where
  def = E.AxisData E.def E.def E.def E.def E.def E.def


instance (E.PlotValue a, RealFloat a, Show a, E.Default (E.AxisData a)) => E.Default (AxisConfig a) where
  def = axisConfDef

exampleAxisConf ::(E.PlotValue y, RealFloat y, Show y, Num y) =>  AxisConfig y
exampleAxisConf =
  let al = E.laxis_title E..~ "Impulse Signal"
           $ E.def
      av = E.axis_show_labels E..~ False
           $ E.axis_show_ticks E..~ False
           $ E.def
      af = E.scaledAxis E.def (-1,10)
  in AxisConfig al av {- E.def -} (Just af)
                  
type LineTy x y = E.EC (E.Layout x y) (E.PlotLines x y)
type LineTyL x y0 y1 = E.EC (E.LayoutLR x y0 y1) (E.PlotLines x y0)
type LineTyR x y0 y1 = E.EC (E.LayoutLR x y0 y1) (E.PlotLines x y1)

data ReportItem =
  forall x y. (E.PlotValue x, E.PlotValue y) => SvgItem Attrs (AxisConfig x) (AxisConfig y, [LineTy x y])
  | forall x y0 y1. (E.PlotValue x, E.PlotValue y0,  RealFloat y0, Show y0, E.PlotValue y1, RealFloat y1, Show y1) => SvgItemLR Attrs (AxisConfig x) (AxisConfig y0, [LineTyL x y0 y1]) (AxisConfig y1, [LineTyR x y0 y1])
  | SvgCandle Attrs String [[E.Candle UTCTime Double]]
  | Paragraph Attrs String
  | Table Attrs Attrs Attrs [[String]]
  | HSplit Attrs ReportItem ReportItem
  
data Report = Report Attrs [ReportItem]

report :: [ReportItem] -> Report
report = Report (toAs [ "font-family" .= "monospace" ])


svg :: (E.PlotValue x, E.PlotValue y) => (AxisConfig x) -> (AxisConfig y, [LineTy x y]) -> ReportItem
svg = SvgItem (toAs [ "clear" .= "both" ])

svgLR ::
  (E.PlotValue x, E.PlotValue y0, RealFloat y0, Show y0, E.PlotValue y1, RealFloat y1, Show y1) =>
  (AxisConfig x) -> (AxisConfig y0, [LineTyL x y0 y1]) -> (AxisConfig y1, [LineTyR x y0 y1]) -> ReportItem
svgLR = SvgItemLR (toAs [ "clear" .= "both" ])

candle :: String -> [Vector (E.Candle UTCTime Double)] -> ReportItem
candle str = SvgCandle (toAs [ "clear" .= "both" ]) str . map Vec.toList

class Line a where
  type TyX a :: *
  type TyY a :: *
  line :: String -> a -> LineTy (TyX a) (TyY a)
  lineL :: String -> a -> LineTyL (TyX a) (TyY a) z
  lineR :: String -> a -> LineTyR (TyX a) z (TyY a)

instance Line (Vector (x, y)) where
  type TyX (Vector (x, y)) = x
  type TyY (Vector (x, y)) = y
  line str vs = E.line str [Vec.toList vs]
  lineL str vs = E.line str [Vec.toList vs]
  lineR str vs = E.line str [Vec.toList vs]

instance Line ([] (x, y)) where
  type TyX ([] (x, y)) = x
  type TyY ([] (x, y)) = y
  line str vs = E.line str [vs]
  lineL str vs = E.line str [vs]
  lineR str vs = E.line str [vs]


class (Show a) => ToText a where
  toText :: a -> ReportItem
  toText = text . show

instance (Show a) => ToText a

text :: String -> ReportItem
text =
  let attrs =
        [ "width" .= "760px"
        , "margin-left" .= "40px"
        , "clear" .= "both" ]
  in Paragraph (toAs attrs)

header :: String -> ReportItem
header =
  let attrs =
        [ "font-size" .= "xx-large"
        , "font-weight" .= "bold"
        , "margin" .= "40px"
        , "margin-bottom" .= "20px"
        , "clear" .= "both" ]
  in Paragraph (toAs attrs)

subheader :: String -> ReportItem
subheader =
  let attrs =
        [ "font-size" .= "large"
        , "font-weight" .= "bold"
        , "margin-left" .= "40px"
        , "margin-bottom" .= "10px"
        , "margin-top" .= "12px"
        , "clear" .= "both" ]
  in Paragraph (toAs attrs)

divTable :: Attrs
divTable = toAs [
  "margin" .= "20px"
  , "margin-left" .= "60px"
  , "display" .= "table"
  , "width" .= "auto"
  , "border-spacing" .= "5px"
  , "float" .= "left"
  , "clear" .= "both" ]

divTableRow :: Attrs
divTableRow = toAs [
  "display" .= "table-row"
  , "width" .= "auto"
  , "clear" .= "both" ]

divTableCol :: Attrs
divTableCol = toAs [
  "float" .= "left"
  , "display" .= "table-column"
  , "width" .= "150px" ]

hSplitTable :: Attrs
hSplitTable = toAs [
  "clear" .= "both"
  ]

mapCol :: Attrs -> [String] -> Builder
mapCol as = mconcat . zipWith f [0::Integer ..] 
  where f 0 = tag2 "div" (attr2str (Map.union divTableCol as)) . B.stringUtf8
        f _ = tag2 "div" (attr2str divTableCol) . B.stringUtf8
  
mapRow :: Attrs -> Attrs -> [[String]] -> Builder
mapRow ras cas = mconcat . zipWith f [0::Integer ..]
  where f 0 = tag2 "div" (attr2str (Map.union ras divTableRow)) . mapCol cas
        f _ = tag2 "div" (attr2str divTableRow) . mapCol cas
        
vtable :: [[String]] -> ReportItem
vtable = Table emptyAttrs emptyAttrs (toAs [ "font-weight" .= "bold" ])

htable :: [[String]] -> ReportItem
htable = Table emptyAttrs (toAs [ "font-weight" .= "bold" ]) emptyAttrs

hsplit :: ReportItem -> ReportItem -> ReportItem
hsplit = HSplit hSplitTable

renderReport :: Report -> IO ByteString
renderReport = fmap B.toLazyByteString . renderRep

renderRep :: Report -> IO Builder
renderRep (Report as is) = do
  items <- mapM renderItem is
  let docType = B.stringUtf8 "<!DOCTYPE html>"
      html = tag2 "html" mempty (hd <> bdy)
      hd = tag2 "head" mempty title
      title = tag2 "title" mempty (B.stringUtf8 "Report")
      bdy = tag2 "body" (attr2str as) (mconcat items)
  return (docType <> html)


colors :: [E.AlphaColour Double]
colors = map E.opaque [ E.red, E.blue, E.green, E.magenta, E.orange, E.darkcyan, E.black, E.gray, E.purple, E.pink ] ++ colors

chartSize :: (Double, Double)
chartSize = (1000, 520)

lines2str :: (E.PlotValue x, E.PlotValue y) => AxisConfig x -> (AxisConfig y, [LineTy x y]) -> IO Builder
lines2str acx (acy, ls) = do
  let fstyle = E.def {
        E._font_name = "monospace"
        , E._font_size = 24
        , E._font_weight = E.FontWeightNormal
        }

      df = E.def { D._fo_format = D.SVG_EMBEDDED
                 , D._fo_fonts = fmap (. (const fstyle)) D.loadCommonFonts
                 , D._fo_size = chartSize }
           
      diagram = do
        E.setColors colors
        
        E.layout_x_axis E..= axisLayout acx
        E.layout_bottom_axis_visibility E..= axisVisibility acx
        case axisFn acx of
          Just x -> E.layout_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        E.layout_y_axis E..= axisLayout acy
        E.layout_left_axis_visibility E..= axisVisibility acy
        case axisFn acy of
          Just x -> E.layout_y_axis . E.laxis_generate E..= x
          Nothing -> return ()

        mapM_ E.plot ls
  
  fmap B.lazyByteString (D.toBS df diagram)

lines2strLR ::
  (E.PlotValue x
  , E.PlotValue y0, RealFloat y0, Show y0, Num y0
  , E.PlotValue y1, RealFloat y1, Show y1, Num y1) =>
  AxisConfig x -> (AxisConfig y0, [LineTyL x y0 y1]) -> (AxisConfig y1, [LineTyR x y0 y1]) -> IO Builder
lines2strLR acx (acL, lsL) (acR, lsR) = do
  let fstyle = E.def {
        E._font_name = "monospace"
        , E._font_size = 24
        , E._font_weight = E.FontWeightNormal
        }

      df = E.def { D._fo_format = D.SVG_EMBEDDED
                 , D._fo_fonts = fmap (. (const fstyle)) D.loadCommonFonts
                 , D._fo_size = chartSize }

      diagram = do
        E.setColors colors

        E.layoutlr_x_axis E..= axisLayout acx
        E.layoutlr_bottom_axis_visibility E..= axisVisibility acx
        case axisFn acx of
          Just x -> E.layoutlr_x_axis . E.laxis_generate E..= x
          Nothing -> return ()

        E.layoutlr_left_axis E..= axisLayout acL
        E.layoutlr_left_axis_visibility E..= axisVisibility acL
        case axisFn acL of
          Just x -> E.layoutlr_left_axis . E.laxis_generate E..= x
          Nothing -> return ()

        E.layoutlr_right_axis E..= axisLayout acR
        E.layoutlr_right_axis_visibility E..= axisVisibility acR
        case axisFn acR of
          Just x -> E.layoutlr_right_axis . E.laxis_generate E..= x
          Nothing -> return ()

        mapM_ E.plotLeft lsL
        mapM_ E.plotRight lsR

  fmap B.lazyByteString (D.toBS df diagram)


toCandle :: String -> [[E.Candle UTCTime Double]] -> IO Builder
toCandle label cs = do
  
  let fstyle = E.def {
        E._font_name = "monospace"
        , E._font_size = 24
        , E._font_weight = E.FontWeightNormal
        }

      df = E.def { D._fo_format = D.SVG_EMBEDDED
                 , D._fo_fonts = fmap (. (const fstyle)) D.loadCommonFonts
                 , D._fo_size = chartSize }

      lineStyle n colour = E.line_width E..~ n
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

        mapM_ (E.plot_candle_values E..=) cs

        E.plot_candle_title E..= label
    
  fmap B.lazyByteString (D.toBS df (E.plot diagram))
  
renderItem :: ReportItem -> IO Builder
renderItem (SvgItem as acx ls) = lines2str acx ls >>= return . tag2 "div" (attr2str as) 
renderItem (SvgItemLR as acy ls0 ls1) =
  lines2strLR acy ls0 ls1 >>= return . tag2 "div" (attr2str as)
renderItem (SvgCandle as label ls) = toCandle label ls >>= return . tag2 "div" (attr2str as) 
renderItem (Paragraph as str) =
  return (tag2 "div" (attr2str as) (B.stringUtf8 str))
renderItem (Table tas ras cas table) =
  return (tag2 "div" (attr2str (Map.union divTable tas)) (mapRow ras cas table))
renderItem (HSplit as i0 i1) = do
  i0' <- renderItem i0
  i1' <- renderItem i1
  return (tag2 "div" (attr2str (Map.union hSplitTable as)) (i0' <> i1'))

