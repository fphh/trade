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
  , barsByTimeSpan :: Bars
  , yield :: LogYield ohlc
  , yldPerBar :: LogYield ohlc
  , vola30 :: Maybe Percent
  , vola90 :: Maybe Percent
  , vola250 :: Maybe Percent
  , timeCoveredByBars :: Percent
  }

sampleStatistics ::
  (ToYield ohlc, Fractional ohlc, Floating ohlc, Statistics ohlc) =>
  BarLength -> Timeseries ohlc -> SampleStatistics ohlc
sampleStatistics barLen xs =
  let ie@(t0, y0) = Signal.head xs
      fe@(tn, yn) = Signal.last xs
      
      timeSpan = (tn `diffUTCTime` t0) + barLength2diffTime barLen
      barsByTimeSpan = Bars (floor (timeSpan / barLength2diffTime barLen))
      yld = toYield timeSpan yn y0
      ypb = yieldPerBar barsByTimeSpan yld

      sampLen = Bars (Signal.length xs)

      vs = Signal.values xs
      vlen = Vec.length vs
      vola n =
        case vlen > n of
          True -> Just (Stats.volatility (Vec.slice (vlen - (n+1)) (n+1) vs))
          False -> Nothing

      tcb = (\(Bars sl) (Bars ts) -> fromIntegral sl / fromIntegral ts) sampLen barsByTimeSpan


  in SampleStatistics {
    barLength = barLen
    , initialEquity = ie
    , finalEquity = fe
    , timeSpan = timeSpan
    , barsByTimeSpan = barsByTimeSpan
    , sampleLength = sampLen
    , yield = yld
    , yldPerBar = ypb
    , vola30 = vola 30
    , vola90 = vola 90
    , vola250 = vola 250
    , timeCoveredByBars = Percent tcb
    }

sampleStatistics2table ::
  (Pretty ohlc) =>
  SampleStatistics ohlc -> [[String]]
sampleStatistics2table ss =
  let format (x, y) = [pretty x, pretty y]
  in [ "Initial" : format (initialEquity ss)
     , "Final" : format (finalEquity ss)
     , [ "Time span", pretty (timeSpan ss) ]
     , [ "Bars by time span", pretty (barsByTimeSpan ss) ]
     , [ "Sample length", pretty (sampleLength ss) ]
     , [ "Bar length", pretty (barLength ss) ]
     , [ "Yield", "", pretty (logYield2yield (yield ss)) ]
     , [ "Yield per bar", "", pretty (logYield2yield (yldPerBar ss)) ]
     , []
     , [ "Log. volatility  30 bars", pretty (vola30 ss) ]
     , [ "Log. volatility  90 bars", pretty (vola90 ss) ]
     , [ "Log. volatility 250 bars", pretty (vola250 ss) ]
     , []
     , [ "Signal Quality" ]
     , [ "Time covered by bars", pretty (timeCoveredByBars ss) ] ]



instance (Pretty ohlc) => ToReport (SampleStatistics ohlc) where
  toReport = Table.table . sampleStatistics2table
