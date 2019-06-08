{-# LANGUAGE FlexibleContexts #-}

module Trade.Statistics.SampleStatistics where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)

import Trade.Type.BarLength (BarLength, Bars(..), barLength2diffTime)
import Trade.Type.Signal (Timeseries)
import qualified Trade.Type.Signal as Signal
import Trade.Type.Yield (LogYield(..), ToYield, toYield, logYield2yield, yieldPerBar)


import Trade.Report.Pretty (Pretty, pretty)
import qualified Trade.Report.Table as Table
import Trade.Report.ToReport (ToReport, toReport)


data SampleStatistics ohlc = SampleStatistics {
  barLength :: BarLength
  , sampleLength :: Bars
  , initialEquity :: (UTCTime, ohlc)
  , finalEquity :: (UTCTime, ohlc)
  , timeSpan :: NominalDiffTime
  , yield :: LogYield ohlc
  , yldPerBar :: LogYield ohlc
  }


sampleStatistics ::
  (ToYield ohlc) =>
  BarLength -> Timeseries ohlc -> SampleStatistics ohlc
sampleStatistics barLen xs =
  let ie@(t0, y0) = Signal.head xs
      fe@(tn, yn) = Signal.last xs
      yld = toYield ts yn y0
      sigLen@(Bars bs) = Bars (Signal.length xs)
      ts = barLength2diffTime barLen * realToFrac bs
      ypb = yieldPerBar sigLen yld
  in SampleStatistics {
    barLength = barLen
    , sampleLength = sigLen
    , initialEquity = ie
    , finalEquity = fe
    , timeSpan = ts
    , yield = yld
    , yldPerBar = ypb
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
     , [ "Bar Length", pretty (barLength ss) ] ]



instance (Pretty ohlc) => ToReport (SampleStatistics ohlc) where
  toReport = Table.table . sampleStatistics2table
