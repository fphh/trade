
module Trade.Report.Render where

import Control.Monad.Trans.Reader (ReaderT(..), reader)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as Text

import qualified Data.ByteString.Lazy.Search as BSS


import Text.Blaze (unsafeLazyByteString)

import Graphics.Svg.Core (renderBS)
import qualified Graphics.Rendering.Chart.Easy as E
import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
import qualified Graphics.Rendering.Chart.Renderable as R

import qualified Diagrams.Prelude as DP
import qualified Diagrams.TwoD as D2

import qualified Diagrams.Backend.SVG as DSVG

import Trade.Report.Config (HtmlReader, Config(..), readUserConfig, UserConfig(..))



-- we need this for removing clip paths, bug in charts???
rr :: BS.ByteString
rr = BS.pack [99,108,105,112,45,112,97,116,104,61]

toSvg :: E.BackendProgram a -> HtmlReader BSL.ByteString
toSvg cb = do
  (w, h) <- readUserConfig chartDimension
  fs <- reader fontSelector
  let env = D.createEnv E.vectorAlignmentFns w h fs
      (d, _, _) = D.runBackendWithGlyphs env cb
      opts = DSVG.SVGOptions (D2.dims2D w h) Nothing Text.empty [] True
      svg = DP.renderDia DSVG.SVG opts d
  return (BSS.replace rr BSL.empty (renderBS svg))


toBackendProgram ::
  (E.ToRenderable r, E.Default r) =>
  E.EC r () -> HtmlReader (E.BackendProgram (E.PickFn ()))
toBackendProgram ec = do
  dim <- readUserConfig chartDimension
  return (R.render (R.toRenderable (E.execEC ec)) dim)


ec2svg :: (E.ToRenderable r, E.Default r) => E.EC r () -> HtmlReader ()
ec2svg ec =
  toBackendProgram ec >>= toSvg >>= ReaderT . const . unsafeLazyByteString


renderable2svg :: R.Renderable a -> HtmlReader ()
renderable2svg r =
  readUserConfig chartDimension >>= toSvg . R.render r >>= ReaderT . const . unsafeLazyByteString

