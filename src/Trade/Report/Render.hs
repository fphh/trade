
module Trade.Report.Render where

import Control.Monad.Reader (ReaderT(..), reader)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as Text

-- import qualified Data.ByteString.Lazy.Search as BSS


import Text.Blaze (unsafeLazyByteString)

import Graphics.Svg.Core (renderBS)
import qualified Graphics.Rendering.Chart.Easy as E
import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
import qualified Graphics.Rendering.Chart.Renderable as R

import qualified Diagrams.Prelude as DP
import qualified Diagrams.TwoD as D2

import qualified Diagrams.Backend.SVG as DSVG

import Trade.Report.Config (HtmlReader, Config(..), readUserConfig, UserConfig(..))


toSvg :: E.BackendProgram a -> HtmlReader BSL.ByteString
toSvg cb = do
  (w, h) <- readUserConfig chartDimension
  fs <- reader fontSelector
  let env = D.createEnv E.vectorAlignmentFns w h fs
      (d, _, _) = D.runBackendWithGlyphs env cb
      opts = DSVG.SVGOptions (D2.dims2D w h) Nothing Text.empty [] True
      svg = DP.renderDia DSVG.SVG opts d
  return (renderBS svg)


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





{- maybe we need this for clip paths ???? -}

{-
-- | TODO: use sockets or pipes?
toBS :: (E.Default l, E.ToRenderable l) => D.FileOptions -> E.EC l () -> IO ByteString
toBS fopts diagram = Temp.withSystemTempFile "svg-" $
  \file h -> do
    hClose h
    D.toFile fopts file diagram
    bs <- BSL.readFile file
    -- remove clip-path attributes; clip-paths seem to be buggy in Charts-lib.
    return (BSS.replace (BS.pack [99,108,105,112,45,112,97,116,104,61]) BSL.empty bs)

-- | TODO: use sockets or pipes?
toBS2 :: D.FileOptions -> Renderable a -> IO ByteString
toBS2 fopts r = Temp.withSystemTempFile "svg-" $
  \file h -> do
    hClose h
    D.renderableToFile fopts file r
    bs <- BSL.readFile file
    -- remove clip-path attributes; clip-paths seem to be buggy in Charts-lib.
    return (BSS.replace (BS.pack [99,108,105,112,45,112,97,116,104,61]) BSL.empty bs)
-}

