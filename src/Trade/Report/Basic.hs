{-# LANGUAGE FlexibleContexts #-}


module Trade.Report.Basic where


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H5A

import Trade.Report.Config (HtmlReader)

import Trade.Report.ToReport (toReport)



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
