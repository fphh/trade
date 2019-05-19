
module Trade.Report.Render where

import Control.Monad (forM_)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Text.Blaze.Renderer.Text as B

import qualified Graphics.Svg as Svg
import Graphics.Svg.Core (renderBS)
import Graphics.SVGFonts.WriteFont (makeSvgFont)


import qualified Graphics.Rendering.Chart.Easy as E
import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
import qualified Graphics.Rendering.Chart.Renderable as R

import qualified Diagrams.Prelude as DP
import qualified Diagrams.TwoD as D2
-- import qualified Diagrams.TwoD.Arc as D2
-- import qualified Diagrams.TwoD.Text as D2


import qualified Diagrams.Backend.SVG as DSVG

width :: Double
width = 600

height :: Double
height = 400

toSvgEmbedded :: D.FontSelector Double -> E.BackendProgram a -> BSL.ByteString
toSvgEmbedded fontSelector cb =
  let env = (D.createEnv E.vectorAlignmentFns width height fontSelector) {- {
        envFontStyle = def { _font_name = "monospace", _font_weight = FontWeightNormal }
        } -}
      (d, _, gs) = D.runBackendWithGlyphs env cb
      fontDefs = Just . Svg.toElement . B.renderMarkup
        $ forM_ (Map.toList gs) $ \((fFam, fSlant, fWeight), usedGs) -> do
        let fs = D.envFontStyle env
        let font = D.envSelectFont env $ fs {
              E._font_name = fFam
              , E._font_slant = fSlant
              , E._font_weight = fWeight
              }
        makeSvgFont font usedGs
      
      opts = DSVG.SVGOptions (D2.dims2D width height) fontDefs Text.empty [] True
      svg = DP.renderDia DSVG.SVG opts d
  in renderBS svg

toBackendProgram :: (E.ToRenderable r, E.Default r) => E.EC r () -> E.BackendProgram (E.PickFn ())
toBackendProgram ec = R.render (R.toRenderable (E.execEC ec)) (width, height)


ec2svg :: (E.ToRenderable r, E.Default r) => E.EC r () -> IO BSL.ByteString
ec2svg ec = do
  fontSelector <- D.loadCommonFonts
  return (toSvgEmbedded fontSelector (toBackendProgram ec))


renderable2svg :: R.Renderable a -> IO BSL.ByteString
renderable2svg r = do
  fontSelector <- D.loadSansSerifFonts -- D.loadCommonFonts
  return (toSvgEmbedded fontSelector (R.render r (width, height)))




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

