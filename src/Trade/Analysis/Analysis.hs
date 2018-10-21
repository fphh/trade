{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}


module Trade.Analysis.Analysis where

import Control.Monad.Trans (liftIO)



import qualified Trade.Type.Bars as B
import Trade.Type.ImpulseGenerator (ImpulseGenerator)

import Trade.Analysis.Optimize (Optimize, OptReportTy, TimeTy, OHLCTy, optimize)
import Trade.Analysis.Backtest (Backtest, BacktestReportTy, backtest)

import qualified Trade.Report.Report as Rep

import Trade.Analysis.ToReport (ToReport, report, OptimizationData(..), BacktestData(..))

data Analysis optInp backInp = Analysis {
  title :: String
  , impulseGenerator :: ImpulseGenerator optInp (TimeTy optInp) (OHLCTy optInp)
  , optimizationInput :: optInp
  , backtestInput :: backInp (TimeTy optInp) (OHLCTy optInp)
  }


-- ---------------------------------------------

analyze ::
  (Ord (TimeTy optInp), B.Time (TimeTy optInp), Optimize optInp, Backtest backInp
  , ToReport (OptimizationData optInp (OptReportTy optInp))
  , ToReport (BacktestData (TimeTy optInp) (OHLCTy optInp) backInp (BacktestReportTy backInp))) =>
  Analysis optInp backInp -> Rep.HtmlT IO ()
analyze (Analysis ttle impGen optInp backInp) = do
  (optImpGen, optOut) <- liftIO (optimize impGen optInp)
  let backOut = backtest optImpGen backInp
  report ttle (OptimizationData optInp optOut) (BacktestData backInp backOut)
