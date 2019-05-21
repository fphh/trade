

module Trade.Analysis.Report where


import Trade.Report.Config (HtmlReader)



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
  Rep.subheader "Backtest Result"
  Rep.text "No optimized impulse generator found. No backtest done."
  
report ::
  (ToReport (OptimizationData optInp optOut)
  , ToReport (BacktestData backInp backOut)) =>
  String -> OptimizationData optInp optOut -> Maybe (BacktestData backInp backOut) -> HtmlReader ()
report ttle opt back =
  let title = Rep.header ttle
      optRep = toReport opt
      backRep = maybe noBacktestDataReport toReport back
  in liftM3 (\a b c -> a <> b <> c) title optRep backRep
