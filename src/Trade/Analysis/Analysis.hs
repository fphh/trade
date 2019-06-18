{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Analysis where




import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator, ImpulseGenerator, RankedStrategies(..))
import Trade.Type.NonEmptyList (NonEmptyList(..))

import Trade.Analysis.Report (OptimizationData(..), BacktestData(..), report)

import Trade.Report.Config (HtmlReader)

import Trade.Report.ToReport (ToReport)


data Analysis optInp backInp optInpTy ohlc = Analysis {
  title :: String
  , impulseGenerator :: ImpulseGenerator optInpTy ohlc
  , optimizationInput :: optInp
  , backtestInput :: backInp
  }


analyzeHelper ::
  ( ToReport (OptimizationData optInp optOut)
  , ToReport (BacktestData backInp backOut)) =>
  (ImpulseGenerator optInpTy ohlc -> optInp -> (RankedStrategies ohlc, optOut))
  -> (NonEmptyList (OptimizedImpulseGenerator ohlc) -> backInp -> backOut)
  -> Analysis optInp backInp optInpTy ohlc
  -> HtmlReader ()
analyzeHelper optimize backtest (Analysis ttle impGen optInp backInp) = do
  
  let (optImpGen, optOut) = optimize impGen optInp
      btData =
        case optImpGen of
          RankedStrategies [] -> Nothing
          RankedStrategies (best:rest) ->
            let backOut = backtest (NonEmptyList best rest) backInp
            in Just (BacktestData backInp backOut)

  report ttle (OptimizationData optInp optOut) btData

