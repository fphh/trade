{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}


module Trade.Analysis.Analysis where

import Control.Monad.Trans (liftIO)

import Trade.Type.ImpulseGenerator (ImpulseGenerator)

import Trade.Analysis.Optimize (Optimize, OptReportTy, optimize)
import Trade.Analysis.Backtest (Backtest, BacktestReportTy, backtest)

import qualified Trade.Report.Report as Rep

import Trade.Analysis.ToReport (ToReport, report, OptimizationData(..), BacktestData(..))

data Analysis ohlc optInp backInp = Analysis {
  title :: String
  , impulseGenerator :: ImpulseGenerator optInp ohlc
  , optimizationInput :: optInp ohlc
  , backtestInput :: backInp ohlc
  }


-- ---------------------------------------------

analyze ::
  (Optimize optInp, Backtest backInp
  , ToReport (OptimizationData ohlc optInp (OptReportTy optInp))
  , ToReport (BacktestData ohlc backInp (BacktestReportTy backInp))) =>
  Analysis ohlc optInp backInp -> Rep.HtmlIO
analyze (Analysis ttle strat optInp backInp) = do
  (optStrat, optOut) <- liftIO (optimize strat optInp)
  let backOut = backtest optStrat backInp
  report ttle (OptimizationData optInp optOut) (BacktestData backInp backOut)
