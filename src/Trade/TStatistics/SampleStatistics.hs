{-# LANGUAGE FlexibleContexts #-}

module Trade.TStatistics.SampleStatistics where

import Trade.Type.Bars (DeltaTy, Add, diff)
import Trade.Type.Signal (Signal)
import qualified Trade.Type.Signal as Signal
import Trade.Type.Yield (LogYield, ToYield, toYield, logYield2yield)


import Trade.Report.HtmlIO (ToHtmlIO, toHtmlIO, HtmlIO)
import Trade.Report.Pretty (Pretty, pretty)
import qualified Trade.Report.Table as Table


data SampleStatisitcs t ohlc = SampleStatistics {
  initialEquity :: (t, ohlc)
  , finalEquity :: (t, ohlc)
  , timeSpan :: DeltaTy t
  , yield :: LogYield (DeltaTy t) ohlc
  }


sampleStatistics :: (Add t, ToYield ohlc) => Signal t ohlc -> SampleStatisitcs t ohlc
sampleStatistics xs =
  let ie@(t0, y0) = Signal.head xs
      fe@(tn, yn) = Signal.last xs
      ts = tn `diff` t0
      yld = toYield ts yn y0
  in SampleStatistics {
    initialEquity = ie
    , finalEquity = fe
    , timeSpan = ts
    , yield = yld
    }

sampleStatistics2table ::
  (Pretty t, Pretty (DeltaTy t), Pretty ohlc) =>
  SampleStatisitcs t ohlc -> [[String]]
sampleStatistics2table ss =
  let format (x, y) = [pretty x, pretty y]
  in [ "Initial" : format (initialEquity ss)
     , "Final" : format (finalEquity ss)
     , [ "Time span", pretty (timeSpan ss) ]
     , [ "Yield", "", pretty (logYield2yield (yield ss)) ] ]

instance (Pretty t, Pretty (DeltaTy t), Pretty ohlc) => ToHtmlIO (SampleStatisitcs t ohlc) where
  toHtmlIO = Table.table . sampleStatistics2table

