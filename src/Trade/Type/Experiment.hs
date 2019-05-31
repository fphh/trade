{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.Experiment where


import Control.Monad.State (State)

import qualified Data.Vector as Vec

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.List as List

import Data.Time.Clock (UTCTime, NominalDiffTime)

import Text.Printf (PrintfArg)

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H5A

import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import Trade.Type.Bars (DeltaTy, BarLength, Add)
import Trade.Type.Delta (ToDelta)
import Trade.Type.DeltaSignal.Algorithm (concatDeltaSignals)

import qualified Trade.Type.DeltaSignal.Algorithm as DSA

import Trade.Type.DeltaTradeList (DeltaTradeList)
import Trade.Type.DisInvest (InvestSignal)
import Trade.Type.Equity (Equity(..))
import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator(..))
import Trade.Type.ImpulseSignal (ImpulseSignal(..))

import qualified Trade.Type.NestedMap as NestedMap

import Trade.Type.Signal (Timeseries, Signal(..))
import qualified Trade.Type.Signal as Signal

import Trade.Type.Step (StepTy)
import Trade.Type.Step.Algorithm (StepFunction)
-- import Trade.Type.Strategy (Long, Short)
import Trade.Type.Trade (TradeList, emptyTradeList)
import Trade.Type.Yield (ToYield)

import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList, impulse2tradeList)
import Trade.Type.Conversion.Invest2Impulse (Invest2Impulse, invest2impulse)

import Trade.Type.Conversion.TradeList2DeltaTradeList (TradeList2DeltaTradeList, tradeList2DeltaTradeList)

import Trade.Strategy.Type (Signals(..), AlignedSignals(..))
import qualified Trade.Strategy.Process as Strategy

import Trade.Report.Line (Line(..))

--import qualified Trade.Report.Report as Rep

import Trade.Report.Basic (subheader, subsubheader, text)
import qualified Trade.Report.Chart as Chart
import qualified Trade.Report.SparkLine as Spark

import qualified Trade.Statistics.SampleStatistics as SS
import qualified Trade.Statistics.Statistics as Stats
import qualified Trade.Statistics.TradeStatistics as TS
import qualified Trade.Statistics.YieldStatistics as YS

import Trade.Report.ToReport (toReport)

import Trade.Help.SafeTail (slast)

import Trade.Report.Config (HtmlReader)
import Trade.Report.Pretty (Pretty)



data Input stgy sym ohlc = Input {
  step :: StepTy stgy
  , initialEquity :: Equity
  , barLength :: NominalDiffTime
  , impulseGenerator :: OptimizedImpulseGenerator ohlc
  , inputSignals :: Map sym (Timeseries ohlc)
  }

data Output stgy sym ohlc = Output {
  impulseSignals :: Map sym (ImpulseSignal stgy)
  , alignedSignals :: AlignedSignals sym ohlc
  , deltaTradeList :: Map sym (DeltaTradeList ohlc)
  , outputSignal :: Map sym (Timeseries Equity)
  }


data Result stgy sym ohlc = Result {
  input :: Input stgy sym ohlc
  , output :: Output stgy sym ohlc
  }


conduct ::
  forall stgy sym ohlc.
  ( Ord sym
  , Show ohlc
  , ToDelta ohlc
  , TradeList2DeltaTradeList stgy
  , Impulse2TradeList stgy
  , Invest2Impulse stgy
  , StepFunction (StepTy stgy)) =>
  Input stgy sym ohlc -> Result stgy sym ohlc
 
conduct inp@(Input stp eqty _ (OptimizedImpulseGenerator impGen) ps) =
  let 
      strategy :: State (Signals sym ohlc) (AlignedSignals sym ohlc, Map sym InvestSignal)
      strategy = impGen ps
      ((asigs, stgy), _) = Strategy.run strategy

      impSigs :: Map sym (ImpulseSignal stgy)
      impSigs = fmap invest2impulse stgy

      f sym isig = maybe emptyTradeList (flip impulse2tradeList isig) (Map.lookup sym ps)
      ts = Map.mapWithKey f impSigs
      dts = fmap tradeList2DeltaTradeList ts

      timeLine = alignedTimes asigs

      out = Output {
        impulseSignals = impSigs
        , alignedSignals = asigs
        , deltaTradeList = dts
        , outputSignal = fmap (Signal.adjust eqty timeLine . concatDeltaSignals stp eqty) dts
        -- , outputSignal = Signal.adjust eqty timeLine (concatDeltaSignals stp eqty dts)
        }
      
  in Result {
    input = inp
    , output = out
    }



tradeStatistics ::
  ( StepFunction (StepTy stgy)
  , PrintfArg ohlc) =>
  StepTy stgy -> DeltaTradeList ohlc -> HtmlReader ()
tradeStatistics stp dtl =

  let sts = DSA.sortDeltaSignals dtl
      sparks = Spark.toSparkLine stp sts
      ystats = YS.toYieldStatistics sts
      tstats = TS.toTradeStatistics sts

      f _ _ ys ts sp = do
        toReport ys
        toReport ts
        toReport (sequence_ sp)

      zs = NestedMap.zipWith3 f ystats tstats sparks

      g pos wl table =
        let sty = H5A.style (H5.stringValue "clear:both;margin:18px;padding-top:24px;color:#006600")
            header = toReport ((H5.div ! sty) (H5.b (H5.preEscapedToHtml (show pos ++ "/" ++ show wl))))
        in [header, table]
        
  in sequence_ (NestedMap.fold g zs)


lastEquity :: Result stgy sym ohlc -> Equity
lastEquity (Result _ out) =
  let xs:_ = Map.elems (outputSignal out)
  in snd (Signal.last xs)

render ::
  forall stgy ohlc sym.
  ( Show sym
  , StepFunction (StepTy stgy)
  , ToYield ohlc
  , Pretty ohlc
  , PlotValue ohlc
  , PrintfArg ohlc
  , Line (Timeseries ohlc)
  , Line (Vec.Vector (UTCTime, ohlc))) =>
  Result stgy sym ohlc -> HtmlReader ()

render (Result inp out) = do
  
  subheader "Experiment"

  subsubheader "Original Signals"
  Chart.input (inputSignals inp)

  subsubheader "Strategy"
  Chart.strategy (impulseSignals out) (alignedSignals out) (outputSignal out)

  subsubheader "Summary"

  --why do we need the signature ???
  let f :: (ToYield x, Pretty x) => sym -> Timeseries x -> HtmlReader () -> HtmlReader ()
      f sym sig acc = do
        text ("Symbol " ++ show sym)
        toReport (SS.sampleStatistics (barLength inp) sig)
        acc

  Map.foldrWithKey' f (return ()) (inputSignals inp)
  Map.foldrWithKey' f (return ()) (outputSignal out)

  subsubheader "Trade statistics"

  -- mapM_ (text . show . snd) (unSignal (head (Map.elems (outputSignal out))))

  let g sym sig acc = do
        text ("Symbole " ++ show sym)
        tradeStatistics (step inp) sig
        acc

  Map.foldrWithKey' g (return ()) (deltaTradeList out)


