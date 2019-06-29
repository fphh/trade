

module Trade.Report.HtmlReader where

import Control.Monad.Trans.Reader (ReaderT(..))

import qualified Data.ByteString.Lazy as BSL

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html.Renderer.Utf8 as HtmlBSL
import qualified Text.Blaze.Html5.Attributes as H5A
import qualified Text.Blaze.Html5 as H5

import Text.Blaze.Internal (MarkupM)

import Trade.Report.Config (Config, defConfig)


type HtmlReader a = ReaderT Config MarkupM a


render :: HtmlReader () -> IO BSL.ByteString
render html = do
  config <- defConfig
  let sty = H5A.style (H5.stringValue "font-family:monospace;padding:20px;")
      bodySty = H5A.style (H5.stringValue "width:20000px")

      innerHtml = runReaderT html config
      
      doc = do
        H5.docType
        H5.html ! sty $ do
          (H5.body ! bodySty) $ innerHtml
          
  return (HtmlBSL.renderHtml doc)
