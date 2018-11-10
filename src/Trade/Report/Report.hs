{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Report.Report where

import Control.Monad (liftM, ap)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO, lift)

import GHC.IO.Handle (hClose)

import qualified System.IO.Temp as Temp

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL

import qualified Data.ByteString.Lazy.Search as BSS

import Data.Char (ord)


import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Time.Clock

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html.Renderer.Utf8 as HtmlBSL
import qualified Text.Blaze.Html5.Attributes as H5A
import Text.Blaze.Internal (MarkupM(..))

import qualified Graphics.Rendering.Chart.Easy as E
import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
-- import qualified Graphics.Rendering.Chart.Backend.Cairo as D

import Trade.Report.Style
import Trade.Report.Line (L(..))

markupValue :: MarkupM a -> a
markupValue m0 = case m0 of
  Parent _ _ _ m1           -> markupValue m1
  CustomParent _ m1         -> markupValue m1
  Leaf _ _ _ x              -> x
  CustomLeaf _ _ x          -> x
  Content _ x               -> x
  Comment _ x               -> x
  Append _ m1               -> markupValue m1
  AddAttribute _ _ _ m1     -> markupValue m1
  AddCustomAttribute _ _ m1 -> markupValue m1
  Empty x                   -> x

newtype HtmlT m a = HtmlT { runHtmlT :: m (MarkupM a) }

instance (Monad m) => Functor (HtmlT m) where
  fmap = liftM

instance (Monad m) => Applicative (HtmlT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (HtmlT m) where
  return = HtmlT . return . Empty

  -- (>>=) :: HtmlT m a -> (a -> HtmlT m b) -> HtmlT m b
  x >>= f = HtmlT $ do
    y <- runHtmlT x
    z <- runHtmlT (f (markupValue y))
    return (Append y z)
    
instance (Monoid (m (MarkupM a))) => Monoid (HtmlT m a) where
  mempty = HtmlT mempty
  mappend (HtmlT x) (HtmlT y) = HtmlT (mappend x y)
  mconcat = HtmlT . mconcat . map runHtmlT

instance (Monoid (m (MarkupM a))) => Semigroup (HtmlT m a) where
  (<>) = mappend

instance MonadTrans HtmlT where
    lift = HtmlT . (liftM Empty)

instance MonadIO m => MonadIO (HtmlT m) where
    liftIO = lift . liftIO

type HtmlIO = HtmlT IO ()

liftHtml :: (MarkupM a -> MarkupM b) -> HtmlT IO a -> HtmlT IO b
liftHtml f (HtmlT h) = HtmlT $ h >>= return . f

clear :: H5.Attribute
clear = H5A.style (H5.stringValue "") -- "clear:both;")

header :: String -> HtmlIO
header = HtmlT . return . (H5.h1 ! clear) . H5.toHtml

subheader :: String -> HtmlIO
subheader = HtmlT . return . (H5.h2 ! clear) . H5.toHtml

subsubheader :: String -> HtmlIO
subsubheader = HtmlT . return . (H5.h3 ! clear) . H5.toHtml

text :: String -> HtmlIO
text = HtmlT . return . H5.p . H5.toHtml

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
      doc = do
        H5.docType
        H5.html ! sty $ do
          H5.body html'
  return (HtmlBSL.renderHtml doc)

chartSize :: (Double, Double)
chartSize = (1000, 520)


colors :: [E.AlphaColour Double]
colors = map E.opaque [ E.red, E.blue, E.green, E.magenta, E.orange, E.darkcyan, E.black, E.gray, E.purple, E.pink ] ++ colors

-- | TODO: use sockets or pipes?
toBS :: (E.Default l, E.ToRenderable l) => D.FileOptions -> E.EC l () -> IO ByteString
toBS fopts diagram = Temp.withSystemTempFile "svg-" $
  \file h -> do
    hClose h
    D.toFile fopts file diagram
    bs <- BSL.readFile file
    return (BSS.replace (BS.pack [99,108,105,112,45,112,97,116,104,61]) BSL.empty bs)

candle :: String -> [Vector (E.Candle UTCTime Double)] -> HtmlIO
candle label cs = HtmlT $ do
  
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

        mapM_ ((E.plot_candle_values E..=) . Vec.toList) cs

        E.plot_candle_title E..= label
    
  fmap H5.unsafeLazyByteString (toBS df (E.plot diagram))


chartLR ::
  (E.PlotValue x, E.PlotValue y1, E.PlotValue y2) =>
  AxisConfig x ->
  (AxisConfig y1, [L [(x, y1)]]) ->
  (AxisConfig y2, [L [(x, y2)]]) ->
  HtmlIO
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

        let toLine (L str vs) = E.line str [vs]
  
        mapM_ E.plotLeft (map toLine lsL)
        mapM_ E.plotRight (map toLine lsR)

  fmap H5.unsafeLazyByteString (toBS df diagram)


chart :: (E.PlotValue x, E.PlotValue y) => AxisConfig x -> (AxisConfig y, [L [(x, y)]]) -> HtmlIO
chart acx (acy, ls) = HtmlT $ do
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
          
        let toLine (L str vs) = E.line str [vs]

        mapM_ (E.plot . toLine) ls
        
  fmap H5.unsafeLazyByteString (toBS df diagram)
