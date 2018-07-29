{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Analysis where

import Trade.Type.ImpulseGenerator (ImpulseGenerator)

import Trade.Analysis.Optimize (Optimize, OptTy, optimize)
import Trade.Analysis.Backtest (Backtest, BackTy, backtest)

import qualified Trade.Report.Report as Report

import Trade.Analysis.ToReport (ToReport, report)

data Analysis impGenInp optInp backInp = Analysis {
  impulseGenerator :: ImpulseGenerator impGenInp
  , optimizationInp :: optInp
  , backtestInp :: backInp
  }


-- ---------------------------------------------

analyze ::
  (Optimize optInp, ToReport (OptTy optInp), Backtest backInp, ToReport (BackTy backInp)) =>
  Analysis impGenInp optInp backInp -> [Report.ReportItem]
analyze (Analysis strat optInp backInp) =
  let (optStrat, optOut) = optimize strat optInp
      backOut = backtest optStrat backInp
  in report optOut backOut
