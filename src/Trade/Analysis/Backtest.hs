{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Backtest where

--import Data.Time.Clock (UTCTime)

--import qualified Data.Vector as Vec

import Trade.Type.ImpulseGenerator (ImpulseGenerator)


--import Trade.Type.Signal (unSignal)
import Trade.Type.Signal.Price (PriceSignal)
import Trade.Type.Signal.Impulse (ImpulseSignal)
import Trade.Type.Signal.Equity (EquitySignal)
import Trade.Type.Equity (Equity(..))
import Trade.Type.OHLC (UnOHLC)
--import Trade.Type.Trade (TradeList)
import Trade.Type.Conversion.Trade2Equity (trade2equity)
import Trade.Type.Conversion.Impulse2Trade (impulse2trade)

import qualified Trade.Report.Report as Report
import Trade.Analysis.ToReport (ToReport, toReport)

{-
backtest2 :: (UnOHLC a) => (ohlc -> a) -> Equity -> TradeList ohlc -> Report.LineTyL UTCTime Double z
backtest2 tradeAt eqty trades =
  Report.lineL "backtest" (Vec.map (fmap unEquity) (unSignal (trade2equity tradeAt eqty trades)))
-}

equitySignal ::
  UnOHLC a =>
  (ohlc -> a) -> Equity -> ImpulseSignal -> PriceSignal ohlc -> EquitySignal
equitySignal tradeAt eqty impSig qs = 
  let ts = impulse2trade qs impSig
  in trade2equity tradeAt eqty ts
  
class Backtest a where
  type BackTy a :: *
  type ImpGenTy a :: *
  backtest :: ImpulseGenerator (ImpGenTy a) -> a -> BackTy a

data NoBacktest = NoBacktest

data NoBacktestReport = NoBacktestReport

instance Backtest NoBacktest where
  type BackTy NoBacktest = NoBacktestReport
  type ImpGenTy NoBacktest = PriceSignal ()
  backtest _ NoBacktest = NoBacktestReport

instance ToReport NoBacktestReport where
  toReport NoBacktestReport = [ Report.text "No backtest done." ]
               

    
