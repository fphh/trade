{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}


module Trade.Analysis.Analysis where

import Control.Monad.Trans (liftIO)

-- import Trade.Type.Bars (Time)
import Trade.Type.ImpulseGenerator (ImpulseGenerator)

import Trade.Analysis.Optimize (Optimize, OptReportTy, optimize)
import Trade.Analysis.Backtest (Backtest, BacktestReportTy, backtest)

import qualified Trade.Report.Report as Rep

import Trade.Analysis.ToReport (ToReport, report, OptimizationData(..), BacktestData(..))

data Analysis ohlc optInp backInp = Analysis {
  title :: String
  , impulseGenerator :: forall t. ImpulseGenerator t optInp ohlc
  , optimizationInput :: optInp ohlc
  , backtestInput :: backInp ohlc
  }


-- ---------------------------------------------


analyze ::
  (Optimize optInp, Backtest backInp
  , ToReport (OptimizationData ohlc optInp (OptReportTy optInp))
  , ToReport (BacktestData ohlc backInp (BacktestReportTy backInp))) =>
  Analysis ohlc optInp backInp -> Rep.HtmlIO
analyze (Analysis ttle impGen optInp backInp) = do
  (optImpGen, optOut) <- liftIO (optimize impGen optInp)
  let backOut = backtest optImpGen backInp
  report ttle (OptimizationData optInp optOut) (BacktestData backInp backOut)
