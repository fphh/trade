

module Trade.Report.Table where

import Control.Applicative (liftA2)

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set

import Text.Printf (printf)


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H5A


import Trade.Report.HtmlIO (HtmlIO, HtmlT(..))

import Debug.Trace

data Cell y x =
  Empty
  | XIndex x
  | YIndex y
  | Value (y, x)

table :: (Ord x, Ord y, Show y, Show x) => Double -> Map (y, x) Double -> HtmlIO
table bias m = do
  let (ys, xs) = unzip (Map.keys m)
      ys' = Set.toList (Set.fromList ys)
      xs' = Set.toList (Set.fromList xs)
      emptyRow = Empty : map XIndex xs'
      rows = emptyRow : map (\y -> YIndex y : map (\x -> Value (y, x)) xs') ys'

      vs = Map.elems m
      mi = List.minimum vs
      ma = List.maximum vs
      stepHi = 256 / (ma-bias)
      stepLo = 256 / (bias - mi)

      cbSty = H5A.style (H5.stringValue "clear:both;")
      leftSty = H5A.style (H5.stringValue ("float:left;border:1px solid #000000;width:60px;"))
      
      format :: Double -> String
      format x =
        let fmt =
              case x of
                y | (-10) < y && y < 10 -> "%1.5f"
                y | (-100) < y && y < 100 -> "%2.4f"
                y | (-1000) < y && y < 1000 -> "%3.3f"
                y | (-10000) < y && y < 10000 -> "%4.2f"
                y | (-100000) < y && y < 100000 -> "%5.1f"
                y -> "%f"
        in printf fmt x
                    
      content v =
        let bg =
              case v > bias of
                True -> "color:#aa0044;font-weight:bold;background:rgb(0," ++ show (round (stepHi*(v-bias))) ++ ", 0);"
                False -> "color:#4400aa;font-weight:bold;background:rgb(" ++ show (round (stepLo*(bias-v))) ++ ", 0, 0);"
            bgMa = "color:#000000;font-weight:bold;background:#bbffbb"
            sty = H5A.style (H5.stringValue (if v == ma then bgMa else bg))
        in (H5.div ! sty) (H5.toHtml (format v))

      na =
        let bg = H5A.style (H5.stringValue "background:rgb(200,200,120);")
        in (H5.div ! bg) (H5.preEscapedToHtml "&nbsp;")

      emp =
        let bg = H5A.style (H5.stringValue "background:rgb(255,255,255);float:left;width:60px;border:1px solid #ffffff;width:60px;")
        in (H5.div ! bg) (H5.preEscapedToHtml "&nbsp;")
    
  
      g Empty = emp
      g (XIndex x) = (H5.div ! leftSty) (H5.toHtml (show x))
      g (YIndex y) = (H5.div ! leftSty) (H5.toHtml (show y))
      g (Value as) = (H5.div ! leftSty) (maybe na content (Map.lookup as m))
      f rs = (H5.div ! cbSty) (mapM_ g rs)

      pad = H5A.style (H5.stringValue "padding-bottom:24px")
      
  HtmlT (return ((H5.div ! pad) (mapM_ f rows)))
