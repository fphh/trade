{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}


module Trade.Analysis.Backtest where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec

import Trade.Type.ImpulseGenerator (ImpulseGenerator)


import Trade.Type.Signal (unSignal)
import Trade.Type.Equity (Equity(..))
import Trade.Type.OHLC (UnOHLC)
import Trade.Type.Trade (TradeList)
import Trade.Type.Conversion.Trade2Equity (trade2equity)

import qualified Trade.Report.Report as Report
import Trade.Analysis.ToReport (ToReport, toReport)

backtest2 :: (UnOHLC a) => (ohlc -> a) -> Equity -> TradeList ohlc -> Report.LineTyL UTCTime Double z
backtest2 tradeAt eqty trades =
  Report.lineL "backtest" (Vec.map (fmap unEquity) (unSignal (trade2equity tradeAt eqty trades)))


class Backtest a where
  type BackTy a :: *
  backtest :: forall inp. ImpulseGenerator inp -> a -> BackTy a


data NoBacktest = NoBacktest

data NoBacktestReport = NoBacktestReport

instance Backtest NoBacktest where
  type BackTy NoBacktest = NoBacktestReport
  backtest _ _ = NoBacktestReport

instance ToReport NoBacktestReport where
  toReport NoBacktestReport =
    [ Report.subheader "Backtest Report"
    , Report.text "No backtest done." ]
               

    
