{-# LANGUAGE FlexibleContexts #-}

module Trade.Statistics.SampleStatistics where

import Trade.Type.Bars (DeltaTy, Add, diff)
import Trade.Type.Signal (Signal)
import qualified Trade.Type.Signal as Signal
import Trade.Type.Yield (LogYield, ToYield, toYield, logYield2yield)


import Trade.Report.Pretty (Pretty, pretty)
import qualified Trade.Report.Table as Table
import Trade.Report.ToReport (ToReport, toReport)


data SampleStatistics t ohlc = SampleStatistics {
  sampleLength :: !Int
  , initialEquity :: (t, ohlc)
  , finalEquity :: (t, ohlc)
  , timeSpan :: DeltaTy t
  , yield :: LogYield (DeltaTy t) ohlc
  }


sampleStatistics :: (Add t, ToYield ohlc) => Signal t ohlc -> SampleStatistics t ohlc
sampleStatistics xs =
  let ie@(t0, y0) = Signal.head xs
      fe@(tn, yn) = Signal.last xs
      ts = tn `diff` t0
      yld = toYield ts yn y0
  in SampleStatistics {
    sampleLength = Signal.length xs
    , initialEquity = ie
    , finalEquity = fe
    , timeSpan = ts
    , yield = yld
    }

sampleStatistics2table ::
  (Pretty t, Pretty (DeltaTy t), Pretty ohlc) =>
  SampleStatistics t ohlc -> [[String]]
sampleStatistics2table ss =
  let format (x, y) = [pretty x, pretty y]
  in [ "Initial" : format (initialEquity ss)
     , "Final" : format (finalEquity ss)
     , [ "Time span", pretty (timeSpan ss) ]
     , [ "Yield", "", pretty (logYield2yield (yield ss)) ]
     , [ "Sample length", pretty (sampleLength ss) ] ]



instance (Pretty t, Pretty (DeltaTy t), Pretty ohlc) => ToReport (SampleStatistics t ohlc) where
  toReport = Table.table . sampleStatistics2table
