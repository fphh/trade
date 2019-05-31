{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Trade.Report.SparkLine where

import Data.Time.Clock (diffUTCTime)


import qualified Data.Vector as Vec

import qualified Statistics.Sample as Sample


import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H5A

import Text.Blaze.Svg11 (mkPath, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Trade.Type.Step.Algorithm

import Trade.Type.DeltaSignal (DeltaSignal(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Signal (Timeseries, Signal(..))
import qualified Trade.Type.Signal as Signal


data Config = Config {
  width :: Int
  , height :: Int
  }

defConfig :: Config
defConfig = Config 120 80
  
type Range = Double -> Double

toRange :: Config -> Timeseries Equity -> Range
toRange conf sig =
  let (_, Equity mi) = Signal.minimum sig
      (_, Equity ma) = Signal.maximum sig
      h = fromIntegral (height conf)
      (lower, upper) = if ma == mi then (0.8, 1.2) else (mi, ma)
      stepH = h / (upper - lower)
  in \y -> (upper-y) * stepH


type Domain = Double -> Double

toDomain :: Config -> [Timeseries Equity] -> (Double, Double, Domain)
toDomain conf sigs =
  let ts = map (\s -> realToFrac (fst (Signal.last s) `diffUTCTime` fst (Signal.head s))) sigs
      tmax = maximum ts
      vs = Vec.fromList ts
      mean = Sample.mean vs
      stdDev = Sample.stdDev vs
      w = fromIntegral (width conf)
  in (mean, stdDev, \x -> x * w / tmax)


svg :: Config -> S.Svg -> S.Svg
svg conf inner =
  S.docTypeSvg
  ! A.version "1.1"
  ! A.width (S.toValue (width conf+1))
  ! A.height (S.toValue (height conf+1))
  $ inner


spark :: Config -> (Double, Double, Domain) -> Timeseries Equity -> S.Svg
spark conf (mean, stdDev, dom) sig@(Signal xs) =
  let t0 = fst (Signal.head sig)
      ran = toRange conf sig
      as = Vec.map (\(t, e) -> (realToFrac (t `diffUTCTime` t0), unEquity e)) xs
      f acc (t, x) =  acc >> l (dom t) (ran x)
      sty = H5A.style (H5.stringValue "stroke:#4444ff;stroke-width:1px;fill:none;")

      coord = do

        S.rect
          ! H5A.style (H5.stringValue "stroke:0px;fill:#ff0000;opacity:0.05;shape-rendering:crispedges;")
          ! A.x (S.toValue (dom (mean - stdDev)))
          ! A.y (S.toValue (0 :: Double))
          ! A.width (S.toValue (dom (2 * stdDev)))
          ! A.height (S.toValue (height conf))

        S.line
          ! H5A.style (H5.stringValue "stroke:#00aa00;shape-rendering:crispedges;")
          ! A.x1 (S.toValue (dom mean))
          ! A.y1 (S.toValue (0 :: Double))
          ! A.x2 (S.toValue (dom mean))
          ! A.y2 (S.toValue (height conf))
        
        S.line
          ! H5A.style (H5.stringValue "stroke:#000000;shape-rendering:crispedges;")
          ! A.x1 (S.toValue (0 :: Double))
          ! A.y1 (S.toValue (ran 1))
          ! A.x2 (S.toValue (width conf))
          ! A.y2 (S.toValue (ran 1))
          
        S.line
          ! H5A.style (H5.stringValue "stroke:#000000;shape-rendering:crispedges;")
          ! A.x1 (S.toValue (0 :: Double))
          ! A.y1 (S.toValue (0 :: Double))
          ! A.x2 (S.toValue (0 :: Double))
          ! A.y2 (S.toValue (height conf))
          
{-
        S.circle
          ! H5A.style (H5.stringValue "fill:#000000;")
          ! A.cx (S.toValue (dom (32914.28571428572)))
          ! A.cy (S.toValue (ran 1))
          ! A.r (S.toValue (5 :: Double))

        (S.text_ (H5.preEscapedToHtml (show lst)))
          ! H5A.style (H5.stringValue "fill:#000000;")
          ! A.x (S.toValue (dom 0))
          ! A.y (S.toValue (ran 1))
          -}
          
  in svg conf $ do
    coord
    S.path ! sty ! A.d (mkPath (Vec.foldl' f (m 0 (ran 1)) as))


toSparkLine ::
  (Functor f, StepFunction step) =>
  step -> f [DeltaSignal ohlc] -> f [S.Svg]
toSparkLine step mp =
  let eqty = Equity 1
      us = fmap (map (stepFunction step eqty)) mp

      f ss =
        let dom = toDomain defConfig ss
        in map (spark defConfig dom) ss

  in fmap f us

