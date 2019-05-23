{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Report where

import Control.Monad (liftM3)

import Trade.Report.Basic (header, subheader, text)
import Trade.Report.Config (HtmlReader)
import Trade.Report.ToReport (ToReport, toReport)



data OptimizationData optInput optOutput = OptimizationData {
  optimizationInput :: optInput
  , optimizationOutput :: optOutput 
  }

data BacktestData backtestInput backtestOutput = BacktestData {
  backtestInput :: backtestInput
  , backtestOutput :: backtestOutput
  }


noBacktestDataReport ::HtmlReader ()
noBacktestDataReport = do
  subheader "Backtest Result"
  text "No optimized impulse generator found. No backtest done."
  
report ::
  (ToReport (OptimizationData optInp optOut)
  , ToReport (BacktestData backInp backOut)) =>
  String -> OptimizationData optInp optOut -> Maybe (BacktestData backInp backOut) -> HtmlReader ()
report ttle opt back =
  let title = header ttle
      optRep = toReport opt
      backRep = maybe noBacktestDataReport toReport back
  in liftM3 (\a b c -> a <> b <> c) title optRep backRep
