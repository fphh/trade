

module Trade.Report.Heatmap where

import Control.Monad.Reader (ReaderT(..))

import Text.Printf (printf)

import qualified Data.List as List

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5.Attributes as H5A

import Trade.Report.Config (HtmlReader)

data Cell y x =
  Empty
  | XIndex x
  | YIndex y
  | Value (y, x)

heatmap :: (Ord x, Ord y, Show y, Show x) => Double -> Map (y, x) Double -> HtmlReader ()
heatmap bias m = do
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
      
      commonCellSty = "float:left;border:1px solid #000000;width:60px;"
            
      cbSty = H5A.style (H5.stringValue "clear:both;")
      leftSty = H5A.style (H5.stringValue commonCellSty)
      
      format :: Double -> String
      format x =
        let fmt =
              case x of
                y | (-10) < y && y < 10 -> "%1.5f"
                y | (-100) < y && y < 100 -> "%2.4f"
                y | (-1000) < y && y < 1000 -> "%3.3f"
                y | (-10000) < y && y < 10000 -> "%4.2f"
                y | (-100000) < y && y < 100000 -> "%5.1f"
                _ -> "%f"
        in printf fmt x
                    
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
        in (H5.div ! H5A.style (H5.stringValue (commonCellSty ++ sty))) (H5.toHtml (format v))

      emp =
        let bg = H5A.style (H5.stringValue (commonCellSty ++ "background:rgb(200,200,120);width:60px;"))
        in (H5.div ! bg) (H5.preEscapedToHtml "&nbsp;")

      g Empty = emp
      g (XIndex x) = (H5.div ! leftSty) (H5.toHtml (show x))
      g (YIndex y) = (H5.div ! leftSty) (H5.toHtml (show y))
      g (Value as) = maybe emp content (Map.lookup as m)

      f rs = (H5.div ! cbSty) (mapM_ g rs)

      pad = H5A.style (H5.stringValue "padding-bottom:24px")
      
  ReaderT (const ((H5.div ! pad) (mapM_ f rows)))

