{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}


module Trade.Analysis.Analysis where


import Control.Monad.Trans (liftIO)

import Trade.Type.ImpulseGenerator (ImpulseGenerator, RankedStrategies(..))
import Trade.Type.NonEmptyList (NonEmptyList(..))

import Trade.Analysis.OHLCData (OHLCDataTy)
import Trade.Analysis.Optimize (Optimize, OptReportTy, OptInpTy, optimize)
import Trade.Analysis.Backtest (Backtest, BacktestReportTy, backtest)

import Trade.Report.HtmlIO (HtmlIO)

import Trade.Analysis.ToReport (ToReport, report, OptimizationData(..), BacktestData(..))
 
data Analysis optInp backInp = Analysis {
  title :: String
  , impulseGenerator :: ImpulseGenerator (OptInpTy optInp) (OHLCDataTy optInp)
  , optimizationInput :: optInp
  , backtestInput :: backInp
  }

-- | This is the main function of the library. It wires everything together.
-- You have to create instances for `Optimize` and `Backtest`
-- and the corresponding `ToReport` instances.
-- Look at the source code of those modules for an easy example, or
-- look at the folder `Example`.
-- You can find the source of a complete example in `Basic`,
-- then look at `Simple` and from there, look at the other examples.

analyze ::
  ( Optimize optInp
  , Backtest backInp
  , ToReport (OptimizationData optInp (OptReportTy optInp))
  , ToReport (BacktestData backInp (BacktestReportTy backInp))
  , OHLCDataTy optInp ~ OHLCDataTy backInp)
  => Analysis optInp backInp -> HtmlIO
analyze (Analysis ttle impGen optInp backInp) = do
  (optImpGen, optOut) <- liftIO (optimize impGen optInp)

  let btData =
        case optImpGen of
          RankedStrategies [] -> Nothing
          RankedStrategies (best:rest) ->
            let backOut = backtest (NonEmptyList best rest) backInp
            in Just (BacktestData backInp backOut)

  report ttle (OptimizationData optInp optOut) btData

{-
analyze (Analysis ttle impGen optInp backInp) = do
  (optImpGen, optOut) <- liftIO (optimize impGen optInp)
  let backOut = backtest optImpGen backInp
  report ttle (OptimizationData optInp optOut) (BacktestData backInp backOut)
-}
