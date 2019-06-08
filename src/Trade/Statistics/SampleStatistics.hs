{-# LANGUAGE FlexibleContexts #-}

module Trade.Statistics.SampleStatistics where

import qualified Data.Vector as Vec

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)

import Trade.Type.BarLength (BarLength, Bars(..), barLength2diffTime)
import Trade.Type.Percent (Percent(..))
import Trade.Type.Signal (Timeseries, Signal(..))
import qualified Trade.Type.Signal as Signal
import Trade.Type.Yield (LogYield(..), ToYield, toYield, logYield2yield, yieldPerBar)


import qualified Trade.Statistics.Algorithm as Stats
import Trade.Statistics.Algorithm (Statistics)

import Trade.Report.Pretty (Pretty, pretty)
import qualified Trade.Report.Table as Table
import Trade.Report.ToReport (ToReport, toReport)


import Debug.Trace


data SampleStatistics ohlc = SampleStatistics {
  barLength :: BarLength
  , sampleLength :: Bars
  , initialEquity :: (UTCTime, ohlc)
  , finalEquity :: (UTCTime, ohlc)
  , timeSpan :: NominalDiffTime
  , yield :: LogYield ohlc
  , yldPerBar :: LogYield ohlc
  , vola30 :: Maybe Percent
  , vola90 :: Maybe Percent
  , vola250 :: Maybe Percent
  , barsAvailable :: Percent
  , isInSync :: Bool
  }

sampleStatistics ::
  (ToYield ohlc, Fractional ohlc, Floating ohlc, Statistics ohlc) =>
  BarLength -> Timeseries ohlc -> SampleStatistics ohlc
sampleStatistics barLen xs =
  let ie@(t0, y0) = Signal.head xs
      fe@(tn, yn) = Signal.last xs
      ts = (tn `diffUTCTime` t0) + barLength2diffTime barLen
      bars = ts / barLength2diffTime barLen
      sigLen = Bars (round bars)
      yld = toYield ts yn y0
      ypb = yieldPerBar sigLen yld

      vs = Signal.values xs
      vlen = Vec.length vs
      vola n =
        case vlen > n of
          True -> Just (Stats.volatility (Vec.slice (vlen - (n+1)) (n+1) vs))
          False -> Nothing

  in SampleStatistics {
    barLength = barLen
    , sampleLength = sigLen
    , initialEquity = ie
    , finalEquity = fe
    , timeSpan = ts
    , yield = yld
    , yldPerBar = ypb
    , vola30 = vola 30
    , vola90 = vola 90
    , vola250 = vola 250
    , barsAvailable = Percent (fromIntegral (Signal.length xs) / fromIntegral (unBars sigLen))
    , isInSync = bars == fromIntegral (round bars)
    }

sampleStatistics2table ::
  (Pretty ohlc) =>
  SampleStatistics ohlc -> [[String]]
sampleStatistics2table ss =
  let format (x, y) = [pretty x, pretty y]
  in [ "Initial" : format (initialEquity ss)
     , "Final" : format (finalEquity ss)
     , [ "Time span", pretty (timeSpan ss) ]
     , [ "Yield", "", pretty (logYield2yield (yield ss)) ]
     , [ "Yield per bar", "", pretty (logYield2yield (yldPerBar ss)) ]
     , [ "Sample length", pretty (sampleLength ss) ]
     , [ "Bar Length", pretty (barLength ss) ]
     , []
     , [ "Log. volatility 30 bars", pretty (vola30 ss) ]
     , [ "Log. volatility  90 bars", pretty (vola90 ss) ]
     , [ "Log. volatility  250 bars", pretty (vola250 ss) ]

     , []
     , [ "Signal Quality" ]
     , [ "Bars available in time span", pretty (barsAvailable ss) ]
     , [ "Time span / bar length is integer", pretty (isInSync ss) ] ]



instance (Pretty ohlc) => ToReport (SampleStatistics ohlc) where
  toReport = Table.table . sampleStatistics2table
