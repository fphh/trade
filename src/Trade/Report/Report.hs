{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Trade.Report.Report where

import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)

import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy (ByteString)

import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Monoid

import Data.Time.Clock

import Trade.Render.Svg.Svg
import Trade.Render.Svg.Plot
import Trade.Render.Svg.Extent
import Trade.Render.Svg.Layout
import Trade.Render.Svg.AxisTicks

import Trade.Render.Common.Attr
import Trade.Render.Common.Utils

data ReportItem =
  SvgItem Attrs (Svg [])
  | SvgItemVec Attrs (Svg Vector)
  | Paragraph Attrs String
  | Table Attrs Attrs Attrs [[String]]
  | HSplit Attrs ReportItem ReportItem

  
data Report = Report Attrs [ReportItem]

report :: [ReportItem] -> Report
report = Report (toAs [ "font-family" .= "monospace" ])


class SvgItem vec where
  svg :: (AxisTicks x, Ord x, Ty (XExtent x) ~ x, Orient (XExtent x) ~ InnerWidth, Scale (XExtent x), Show x) => [PlotItem vec x] -> ReportItem

instance SvgItem [] where
  svg = SvgItem (toAs [ "clear" .= "both" ]) . toSvg

instance SvgItem Vector where
  svg = SvgItemVec (toAs [ "clear" .= "both" ]) . toSvg


class (Show a) => ToText a where
  toText :: a -> ReportItem
  toText = text . show

instance (Show a) => ToText a

text :: String -> ReportItem
text = Paragraph (toAs [ "width" .= "760px", "margin-left" .= "40px", "clear" .= "both" ])

header :: String -> ReportItem
header = Paragraph (toAs [ "font-size" .= "xx-large", "font-weight" .= "bold", "margin" .= "40px", "margin-bottom" .= "20px", "clear" .= "both" ])

subheader :: String -> ReportItem
subheader = Paragraph (toAs [ "font-size" .= "large", "font-weight" .= "bold", "margin-left" .= "40px", "margin-bottom" .= "10px", "clear" .= "both" ])


divTable = toAs [
  "margin" .= "20px"
  , "margin-left" .= "60px"
  , "display" .= "table"
  , "width" .= "auto"
  , "border-spacing" .= "5px"
  , "float" .= "left"
  , "clear" .= "both" ]

divTableRow = toAs [
  "display" .= "table-row"
  , "width" .= "auto"
  , "clear" .= "both" ]

divTableCol = toAs [
  "float" .= "left"
  , "display" .= "table-column"
  , "width" .= "200px" ]


hSplitTable = toAs [
  "clear" .= "both"
  ]

mapCol :: Attrs -> [String] -> Builder
mapCol as = mconcat . zipWith f [0..] 
  where f 0 = tag2 "div" (attr2str (Map.union divTableCol as)) . B.stringUtf8
        f n = tag2 "div" (attr2str divTableCol) . B.stringUtf8
  
mapRow :: Attrs -> Attrs -> [[String]] -> Builder
mapRow ras cas = mconcat . zipWith f [0..]
  where f 0 = tag2 "div" (attr2str (Map.union ras divTableRow)) . mapCol cas
        f n = tag2 "div" (attr2str divTableRow) . mapCol cas
        
vtable :: [[String]] -> ReportItem
vtable = Table emptyAttrs emptyAttrs (toAs [ "font-weight" .= "bold" ])

htable :: [[String]] -> ReportItem
htable = Table emptyAttrs (toAs [ "font-weight" .= "bold" ]) emptyAttrs

hsplit :: ReportItem -> ReportItem -> ReportItem
hsplit = HSplit hSplitTable

renderReport :: Report -> ByteString
renderReport = B.toLazyByteString . renderRep

renderRep :: Report -> Builder
renderRep (Report as is) =
  let docType = B.stringUtf8 "<!DOCTYPE html>"
      html = tag2 "html" mempty (head <> body)
      head = tag2 "head" mempty title
      title = tag2 "title" mempty (B.stringUtf8 "Report")
      body = tag2 "body" (attr2str as) (mconcat $ map renderItem is)
  in html

renderItem :: ReportItem -> Builder
renderItem (SvgItem as svg) =
  tag2 "div" (attr2str as) (svg2str svg)
renderItem (SvgItemVec as svg) =
  tag2 "div" (attr2str as) (svg2str svg)
renderItem (Paragraph as str) =
  tag2 "div" (attr2str as) (B.stringUtf8 str)
renderItem (Table tas ras cas table) =
  tag2 "div" (attr2str (Map.union divTable tas)) (mapRow ras cas table)
renderItem (HSplit as i0 i1) =
  tag2 "div" (attr2str (Map.union hSplitTable as)) (renderItem i0 <> renderItem i1)
