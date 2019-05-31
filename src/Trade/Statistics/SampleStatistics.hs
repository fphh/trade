{-# LANGUAGE FlexibleContexts #-}

module Trade.Statistics.SampleStatistics where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)


import Trade.Type.Signal (Timeseries)
import qualified Trade.Type.Signal as Signal
import Trade.Type.Yield (LogYield(..), ToYield, toYield, logYield2yield)


import Trade.Report.Pretty (Pretty, pretty)
import qualified Trade.Report.Table as Table
import Trade.Report.ToReport (ToReport, toReport)


data SampleStatistics ohlc = SampleStatistics {
  sampleLength :: !Int
  , initialEquity :: (UTCTime, ohlc)
  , finalEquity :: (UTCTime, ohlc)
  , timeSpan :: NominalDiffTime
  , yield :: LogYield ohlc
  , yieldPerBar :: LogYield ohlc
  }


sampleStatistics ::
  (ToYield ohlc) =>
  NominalDiffTime -> Timeseries ohlc -> SampleStatistics ohlc
sampleStatistics barLen xs =
  let ie@(t0, y0) = Signal.head xs
      fe@(tn, yn) = Signal.last xs
      ts = tn `diffUTCTime` t0
      yld@(LogYield _ y) = toYield ts yn y0
      yldPerBar = LogYield barLen (y / fromIntegral (Signal.length xs))
  in SampleStatistics {
    sampleLength = Signal.length xs
    , initialEquity = ie
    , finalEquity = fe
    , timeSpan = ts
    , yield = yld
    , yieldPerBar = yldPerBar
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
     , [ "Yield per bar", "", pretty (logYield2yield (yieldPerBar ss)) ]
     , [ "Sample length", pretty (sampleLength ss) ] ]



instance (Pretty ohlc) => ToReport (SampleStatistics ohlc) where
  toReport = Table.table . sampleStatistics2table
