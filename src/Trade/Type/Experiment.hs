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

import Trade.Type.Signal (Signal(..))
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



data Input stgy sym t ohlc = Input {
  step :: StepTy stgy t
  , initialEquity :: Equity
  , barLength :: DeltaTy t
  , impulseGenerator :: OptimizedImpulseGenerator ohlc
  , inputSignals :: Map sym (Signal t ohlc)
  }

data Output stgy sym t ohlc = Output {
  impulseSignals :: Map sym (ImpulseSignal stgy t)
  , alignedSignals :: AlignedSignals sym t ohlc
  , deltaTradeList :: Map sym (DeltaTradeList t ohlc)
  , outputSignal :: Map sym (Signal t Equity)
  }


data Result stgy sym t ohlc = Result {
  input :: Input stgy sym t ohlc
  , output :: Output stgy sym t ohlc
  }


conduct ::
  forall stgy sym t ohlc.
  ( Ord sym
  , Ord t
  , Add t
  , Show t, Show ohlc
  , ToDelta ohlc
  , TradeList2DeltaTradeList stgy
  , Impulse2TradeList stgy
  , Invest2Impulse stgy
  , StepFunction (StepTy stgy) t) =>
  Input stgy sym t ohlc -> Result stgy sym t ohlc
 
conduct inp@(Input stp eqty _ (OptimizedImpulseGenerator impGen) ps) =
  let 
      strategy :: State (Signals sym t ohlc) (AlignedSignals sym t ohlc, Map sym (InvestSignal t))
      strategy = impGen ps
      ((asigs, stgy), _) = Strategy.run strategy

      impSigs :: Map sym (ImpulseSignal stgy t)
      impSigs = fmap invest2impulse stgy

{-
      impSig =
        case Map.elems impSigs of
          [] -> ImpulseSignal Map.empty
          x:_ -> x
-}

      f sym isig = maybe emptyTradeList (flip impulse2tradeList isig) (Map.lookup sym ps)
      ts :: Map sym (TradeList stgy t ohlc)
      ts = Map.mapWithKey f impSigs
      
      -- ts = impulse2tradeList (snd ps) impSig
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
  ( Eq t
  , Add t
  , StepFunction (StepTy stgy) t
  , Real (DeltaTy t)
  , Pretty (DeltaTy t)
  , Pretty (Stats.DeltaTyStats t)) =>
  StepTy stgy t -> DeltaTradeList t ohlc -> HtmlReader ()

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


lastEquity :: Result stgy sym t ohlc -> Equity
lastEquity (Result _ out) =
  let xs:_ = Map.elems (outputSignal out)
  in snd (Signal.last xs)

render ::
  forall stgy t ohlc sym.
  ( Show sym
  , Pretty t
  , Add t
  , StepFunction (StepTy stgy) t
  , PlotValue t
  , Pretty (DeltaTy t)
  , Real (DeltaTy t)
  , Pretty (Stats.DeltaTyStats t)
  , ToYield ohlc
  , Pretty ohlc
  , PlotValue ohlc
  , Line (Signal t ohlc)
  , Line (Vec.Vector (t, ohlc))) =>
  Result stgy sym t ohlc -> HtmlReader ()

render (Result inp out) = do
  
  subheader "Experiment"
      
  Chart.strategy (impulseSignals out) (alignedSignals out) (outputSignal out)

  subsubheader "Summary"

  let -- why do we need the signature ???
      f :: (ToYield x, Pretty x) => sym -> Signal t x -> HtmlReader () -> HtmlReader ()
      f sym sig acc = do
        text ("Symbol " ++ show sym)
        toReport (SS.sampleStatistics (barLength inp) sig)
        acc

  Map.foldrWithKey' f (return ()) (inputSignals inp)
  Map.foldrWithKey' f (return ()) (outputSignal out)

  subsubheader "Trade statistics"



  let g sym sig acc = do
        text ("Symbole " ++ show sym)
        tradeStatistics (step inp) sig
        acc

  Map.foldrWithKey' g (return ()) (deltaTradeList out)


