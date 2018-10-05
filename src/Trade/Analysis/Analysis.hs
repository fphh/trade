{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Analysis where

import Trade.Type.ImpulseGenerator (ImpulseGenerator)

import Trade.Analysis.Optimize (Optimize, OptTy, optimize)
import Trade.Analysis.Backtest (Backtest, BackTy, ImpGenTy, backtest)

import qualified Trade.Report.Report as Report

import Trade.Analysis.ToReport (ToReport, report, OptimizationData(..), BacktestData(..))

data Analysis optInp backInp = Analysis {
  title :: String
  , impulseGenerator :: ImpulseGenerator (ImpGenTy backInp)
  , optimizationInput :: optInp
  , backtestInput :: backInp
  }


-- ---------------------------------------------

analyze ::
  (Optimize optInp, Backtest backInp
  , ToReport (OptimizationData optInp (OptTy optInp))
  , ToReport (BacktestData backInp (BackTy backInp))) =>
  Analysis optInp backInp -> [Report.ReportItem]
analyze (Analysis ttle strat optInp backInp) =
  let (optStrat, optOut) = optimize strat optInp
      backOut = backtest optStrat backInp
  in report ttle (OptimizationData optInp optOut) (BacktestData backInp backOut)
