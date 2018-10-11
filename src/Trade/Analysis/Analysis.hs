{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}


module Trade.Analysis.Analysis where

-- import Text.Blaze.Html5 (Html)

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

{-
analyze ::
  (Optimize (optInp ohlc), Backtest (backInp ohlc)
  , ToReport (OptimizationData (optInp ohlc) (OptReportTy (optInp ohlc)))
  , ToReport (BacktestData (backInp ohlc) (BacktestReportTy (backInp ohlc)) ohlc)) =>
  Analysis ohlc optInp backInp -> Rep.HtmlIO
  -}

analyze ::
  (Optimize optInp, Backtest backInp
  , ToReport (OptimizationData ohlc optInp (OptReportTy optInp))
  , ToReport (BacktestData ohlc backInp (BacktestReportTy backInp))) =>
  Analysis ohlc optInp backInp -> Rep.HtmlIO
analyze (Analysis ttle strat optInp backInp) =
  let (optStrat, optOut) = optimize strat optInp
      backOut = backtest optStrat backInp
  in report ttle (OptimizationData optInp optOut) (BacktestData backInp backOut)
