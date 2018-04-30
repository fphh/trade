{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Trade.Svg.Plot where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as B

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List

import Text.Printf (printf, PrintfArg)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Data.Time.Calendar (Day(..))

import Trade.Svg.Svg
import Trade.Svg.Layout
import Trade.Svg.Extent
import Trade.Svg.DrawingVec
import Trade.Svg.AxisTicks

import Common.Attr
import Common.Color

import Debug.Trace


data PlotItem vec x =
  Line String (vec (x, Double))
  | Scatter String (vec (x, Double))
  | Histogram String (Vector x, Vector Integer)
  

data LinePlot vec x = LinePlot {
  layout :: Layout
  , xAxisLabel :: Maybe String
  , yAxisLabel :: Maybe String
  , curves :: [PlotItem vec x]
  , xScale :: (x -> Double)
  , xExtent :: XExtent x
  , yScale :: (Double -> Double)
  , yExtent :: YExtent
  , marker :: Bool
  }

toLinePlot ::
  (DrawingVec vec, Orient (XExtent x) ~ InnerWidth, Ty (XExtent x) ~ x, Scale (XExtent x), Ord x)
  => Maybe String -> Maybe String -> Layout -> [PlotItem vec x] -> LinePlot vec x
toLinePlot xLabel yLabel layout@Layout{..} curves =
  let (xe, ye) = xyExtent (vecConcat $ map f curves)
      f (Line _ xs) = xs
      f (Scatter _ xs) = xs
      f (Histogram _ (xs, ys)) = vecFromList
        $ Vec.toList
        $ Vec.zipWith (\x y -> (x, fromIntegral y)) xs ys

  in LinePlot {
    layout = layout
    , xAxisLabel = xLabel
    , yAxisLabel = yLabel
    , curves = curves
    , xScale = scale xe (innerWidth layout)
    , xExtent = xe
    , yScale = scale ye (innerHeight layout)
    , yExtent = ye
    , marker = False }



defLinePlot ::
  (DrawingVec vec, Orient (XExtent x) ~ InnerWidth, Ty (XExtent x) ~ x, Scale (XExtent x), Ord x)
  => [PlotItem vec x] -> LinePlot vec x
defLinePlot = toLinePlot Nothing Nothing defaultLayout


plotlines :: (DrawingVec vec) => LinePlot vec x -> Drawing vec
plotlines LinePlot{..} =
  let attr c = toAs [ "stroke" .= c, "fill" .= "none"]
      txtAttr c = toAs [ "fill" .= c, "dominant-baseline" .= "central" ]
      
      circAttr c = toAs [
        "stroke" .= "black"
        , "fill" .= c
        , "fill-opacity" .= (0.4 :: Double) ]

      histAttr c = toAs [
        "stroke" .= Black
        , "fill-opacity" .= (0.4 :: Double)
        , "stroke-width" .= (1 :: Int)
        , "vector-effect" .= "non-scaling-stroke"
        , "fill" .= c ]


      InnerHeight ih = innerHeight layout

      scalePts (x, y) = (xScale x, yScale y)
      xspace (x, y) = (x+5, y)

      -- polyline :: Color -> PlotItem vec x -> Drawing vec
      --polyline :: (DrawingVec vec) => Color -> PlotItem vec -> Drawing vec
      -- polyline c (Line _ ds) | vecNull ds = Empty

      polyline c (Line str ds) =
        let pts = vecMap scalePts ds
            l = PolyLine (attr c) pts
            t = Text (txtAttr c) (xspace (vecLast pts)) str
            -- to slow:
            -- toCircle pt (x, y) = Circle (circAttr c) 2 pt (printf "x = %.2f\ny = %.2f" x y)
            -- cs = vecZipWith toCircle pts ds
        in g_ (toAs [ "id" .= str ]) ([l,t] {- ++ (if marker then vecToList cs else []) -})


      polyline c (Scatter str ds) =
        let pts = vecMap scalePts ds
            toCircle pt (_x, _y) = Circle (circAttr c) 3 pt "" -- (printf "x = %.2f\ny = %.2f" x y)
            cs = vecZipWith toCircle pts ds
        in g_ (toAs [ "id" .= str ]) (vecToList cs)
        
      polyline c (Histogram str (bnds, smpls)) =
        let pts = Vec.map (yScale . fromIntegral) smpls
            bs = Vec.map xScale bnds
            b0 = bs Vec.! 0
            b1 = bs Vec.! 1
            lw = b1-b0-4
            f b s = PolyLine (histAttr c)
              $ vecFromList [(b, ih), (b+lw, ih), (b+lw, s), (b, s), (b, ih)]

        in g_ (toAs [ "id" .= str ]) (Vec.toList $ Vec.zipWith f bs pts)

  in g_ (toAs [ "id" .= "plotlines" ]) (zipWith polyline colors curves)


tickStyle :: Attrs
tickStyle = toAs ["stroke" .= "black"]
      
xticks :: (DrawingVec vec, AxisTicks x, Ord x) => LinePlot vec x -> Drawing vec
xticks LinePlot{..} =
  let txtAttr = toAs ["fill" .= "black", "dominant-baseline" .= "central", "text-anchor" .= "middle"]
      len = 5
      textDist = 7
      InnerHeight ih = innerHeight layout
      InnerWidth iw = innerWidth layout
      
      toTick t =
        let x = xScale t
        in PolyLine tickStyle (vecFromList [(x, ih), (x, ih+len)])

      toText t =
        let x = xScale t
        in Text txtAttr (x, ih+len+textDist) (format t)
        
      (format, ts) = axisTicks (xmin xExtent) (xmax xExtent)
      xs = vecFromList (map toTick ts)
      txts = vecFromList (map toText ts)
      
      xaxis = PolyLine tickStyle (vecFromList [(0, ih), (iw, ih)])

  in g_ (toAs [ "class" .= "x-axis" ]) (xaxis : xs ++ txts)


yticks :: (DrawingVec vec) => LinePlot vec x -> Drawing vec
yticks LinePlot{..} =
  let txtAttr = toAs ["fill" .= "black", "dominant-baseline" .= "central", "text-anchor" .= "end"]
      len = -5
      textDist = 4
      InnerHeight ih = innerHeight layout

      toTick t =
        let y = yScale t
        in PolyLine tickStyle (vecFromList [(0, y), (len, y)])

      toText t =
        let y = yScale t
        in Text txtAttr (len-textDist, y) (format t)
        
      (format, ts) = axisTicks (ymin yExtent) (ymax yExtent)
      ys = vecFromList (map toTick ts)
      txts = vecFromList (map toText ts)

      yaxis = PolyLine tickStyle (vecFromList [(0, 0), (0, ih)])
      
  in g_ (toAs [ "class" .= "y-axis" ]) (yaxis : ys ++ txts)
  

axes :: (DrawingVec vec, AxisTicks x, Ord x) => LinePlot vec x -> [Drawing vec]
axes lp = [xticks lp, yticks lp]

lineplot ::(DrawingVec vec, AxisTicks x, Ord x) => LinePlot vec x -> Drawing vec
lineplot lp@(LinePlot{..}) =
  let XMargin tx = xMargin layout
      YMargin ty = yMargin layout
      as = axes lp
      ls = plotlines lp
  in G (toAs ["class" .= "lineplot"]) (tx, ty) (ls : as)

toSvg ::
  (DrawingVec vec, AxisTicks x, Ord x, Ty (XExtent x) ~ x, Orient (XExtent x) ~ InnerWidth, Scale (XExtent x))
  => [PlotItem vec x] -> Svg vec
toSvg curves =
  let lp@(LinePlot{layout}) = (defLinePlot curves) -- { marker = True }
      Width w = width layout
      Height h = height layout
      attrs = toAs [ "width" .= w, "height" .= h, "font-family" .= "monospace" ]
  in Svg attrs [lineplot lp]

plot ::
  (DrawingVec vec, AxisTicks x, Ord x, ToPolyLine vec, Ty (XExtent x) ~ x, Orient (XExtent x) ~ InnerWidth, Scale (XExtent x))
  => [PlotItem vec x] -> ByteString
plot = B.toLazyByteString . svg2str . toSvg

