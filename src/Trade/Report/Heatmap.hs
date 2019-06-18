

module Trade.Report.Heatmap where

import Control.Monad (join)

import qualified Data.Text.Lazy as Text

import Formatting (format, fixed)

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set

import Data.Maybe (catMaybes)


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H5A

import Trade.Report.ToReport (toReport)

import Trade.Report.Config (HtmlReader)

data Cell y x =
  Empty
  | XIndex x
  | YIndex y
  | Value (y, x)

heatmap :: (Ord x, Ord y, Show y, Show x) => Double -> Map (y, x) (Maybe Double) -> HtmlReader ()
heatmap bias m =
  let (ys, xs) = unzip (Map.keys m)
      ys' = Set.toList (Set.fromList ys)
      xs' = Set.toList (Set.fromList xs)
      emptyRow = Empty : map XIndex xs'
      rows = emptyRow : map (\y -> YIndex y : map (\x -> Value (y, x)) xs') ys'

      vs = catMaybes (Map.elems m)
      mi = List.minimum vs
      ma = List.maximum vs
      stepHi = 256 / (ma-bias)
      stepLo = 256 / (bias - mi)

      width = 80
      
      commonCellSty = "float:left;border:1px solid #000000;width:" ++ show width ++ "px;"
            
      cbSty = H5A.style (H5.stringValue "clear:both;")
      leftSty = H5A.style (H5.stringValue commonCellSty)
      
      fmt :: Double -> String
      fmt x =
        let maxDec = 6
            dec =
              case round (maxDec - log (abs x) / log 10) of
                n | n <= 0 -> 0
                n -> n
                
        in Text.unpack (format (fixed dec) x)

      content v =
        let bg =
              case v > bias of
                True -> "color:#aa0044;font-weight:bold;background:rgb(0,"
                        ++ show (round (stepHi*(v-bias)) :: Int)
                        ++ ", 0);"
                False -> "color:#4400aa;font-weight:bold;background:rgb("
                         ++ show (round (stepLo*(bias-v)) :: Int)
                         ++ ", 0, 0);"
                         
            bgMa = "color:#000000;font-weight:bold;background:#bbffbb"
            bgMi = "color:#000000;font-weight:bold;background:#ffbbbb"
            sty =
              case (v == ma, v == mi) of
                (True, _) -> bgMa
                (_, True) -> bgMi
                _ -> bg
        in (H5.div ! H5A.style (H5.stringValue (commonCellSty ++ sty))) (H5.toHtml (fmt v))

      emp =
        let bg = H5A.style (H5.stringValue (commonCellSty ++ "background:rgb(200,200,120);width:" ++ show width ++ "px;"))
        in (H5.div ! bg) (H5.preEscapedToHtml "&nbsp;")

      g Empty = emp
      g (XIndex x) = (H5.div ! leftSty) (H5.toHtml (show x))
      g (YIndex y) = (H5.div ! leftSty) (H5.toHtml (show y))
      g (Value as) = maybe emp content (join (Map.lookup as m))

      f rs = (H5.div ! cbSty) (mapM_ g rs)

      pad = H5A.style (H5.stringValue "padding-bottom:24px")
      
  in toReport ((H5.div ! pad) (mapM_ f rows))

