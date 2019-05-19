{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Trade.Type.Experiment where

import Control.Monad.State (State)

import qualified Data.Vector as Vec

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Text.Blaze.Html5 as H5
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as H5A

import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import Trade.Type.Bars (DeltaTy, Add)
import Trade.Type.Delta (Delta(..), ToDelta)
import Trade.Type.DeltaSignal.Algorithm (concatDeltaSignals)

import qualified Trade.Type.DeltaSignal.Algorithm as DSA

import Trade.Type.DeltaTradeList (DeltaTradeList)
import Trade.Type.DisInvest (InvestSignal)
import Trade.Type.Equity (Equity(..))
import Trade.Type.ImpulseGenerator (OptimizedImpulseGenerator(..))
import Trade.Type.ImpulseSignal (ImpulseSignal(..), curve)

import qualified Trade.Type.NestedMap as NestedMap

import Trade.Type.Signal (Signal(..))
-- import qualified Trade.Type.Signal as Signal

import Trade.Type.Step (StepTy)
import Trade.Type.Step.Algorithm (StepFunction)
-- import Trade.Type.Strategy (Long, Short)
import Trade.Type.Trade (TradeList)
import Trade.Type.Yield (ToYield)

import Trade.Type.Conversion.Impulse2TradeList (Impulse2TradeList, impulse2tradeList)
import Trade.Type.Conversion.Invest2Impulse (Invest2Impulse, invest2impulse)

import Trade.Type.Conversion.TradeList2DeltaTradeList (TradeList2DeltaTradeList, tradeList2DeltaTradeList)

import Trade.Strategy.Type (Signals(..), AlignedSignals(..))
import qualified Trade.Strategy.Process as Strategy
import qualified Trade.Strategy.Report as SRep

import qualified Trade.Report.Line as Line
import Trade.Report.Line (Line(..))

import qualified Trade.Report.Report as Rep
import qualified Trade.Report.SparkLine as Spark
import qualified Trade.Report.Style as Style

import qualified Trade.TStatistics.SampleStatistics as SS
import qualified Trade.TStatistics.Statistics as Stats
import qualified Trade.TStatistics.TradeStatistics as TS
import qualified Trade.TStatistics.YieldStatistics as YS


import Trade.Help.SafeTail (slast)

import Trade.Report.HtmlIO (liftHtml, toHtmlIO, HtmlIO)
import Trade.Report.Pretty (Pretty)

import Debug.Trace


data Input stgy sym t ohlc = Input {
  step :: StepTy stgy t
  , initialEquity :: Equity
  , impulseGenerator :: OptimizedImpulseGenerator ohlc
  , inputSignals :: [(sym, Signal t ohlc)]
  }

data Output stgy sym t ohlc = Output {
  impulseSignals :: Map sym (ImpulseSignal stgy t)
  , alignedSignals :: AlignedSignals sym t ohlc
  , deltaTradeList :: DeltaTradeList t ohlc
  , outputSignal :: Signal t Equity
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
  , ToDelta ohlc
  , TradeList2DeltaTradeList stgy
  , Impulse2TradeList stgy
  , Invest2Impulse stgy
  , StepFunction (StepTy stgy) t) =>
  Input stgy sym t ohlc -> Result stgy sym t ohlc
 
conduct inp@(Input stp eqty (OptimizedImpulseGenerator impGen) (ps:_)) =
  let 
      strategy :: State (Signals sym t ohlc) (AlignedSignals sym t ohlc, Map sym (InvestSignal t))
      strategy = impGen [ps]
      ((asigs, stgy), _) = Strategy.run strategy

      impSigs :: Map sym (ImpulseSignal stgy t)
      impSigs = fmap invest2impulse stgy

      impSig =
        case Map.elems impSigs of
          [] -> ImpulseSignal Map.empty
          x:_ -> x

--      impSig:_ = trace (show impSigs) Map.elems impSigs

      ts :: TradeList stgy t ohlc
      ts = impulse2tradeList (snd ps) impSig
      dts = tradeList2DeltaTradeList ts

      out = Output {
        impulseSignals = impSigs
        , alignedSignals = asigs
        , deltaTradeList = dts
        , outputSignal = concatDeltaSignals stp eqty dts
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
  StepTy stgy t -> DeltaTradeList t ohlc -> HtmlIO
tradeStatistics stp dtl =

  let sts = DSA.sortDeltaSignals dtl
      sparks = Spark.toSparkLine stp sts
      ystats = YS.toYieldStatistics sts
      tstats = TS.toTradeStatistics sts

      f _ _ ys ts sp = toHtmlIO ys <> toHtmlIO ts <> toHtmlIO sp
      zs = NestedMap.zipWith3 f ystats tstats sparks

      g pos wl htmlio =
        let sty = H5A.style (H5.stringValue "clear:both;margin:18px;padding-top:24px;color:#006600")
            header = (H5.div ! sty) (H5.b (H5.preEscapedToHtml (show pos ++ "/" ++ show wl)))
        in liftHtml (header <>) htmlio
        
  in NestedMap.fold g zs
  

lastEquity :: Result stgy sym t ohlc -> Equity
lastEquity (Result _ out) = snd (slast "Experiment.lastEquity" (unSignal (outputSignal out)))

{-
render ::
  ( Ord t
  , PlotValue t
  , StepFunction (StepTy stgy) t
  , Pretty t, Pretty (DeltaTy t)
  , Pretty (Stats.DeltaTyStats t), Pretty ohlc
  , ToYield ohlc
  , Add t, Ord (Delta ohlc)
  , Num (DeltaTy t)
  , Real (DeltaTy t)
  , Line.TyX (Signal t ohlc) ~ t
  , Line.TyY (Signal t ohlc) ~ Double
  , Line.Line (Signal t ohlc)) =>
  String -> String -> Result stgy sym t ohlc -> HtmlIO
-}
render ::
  ( Show sym
  , Pretty t
  , Add t
  , StepFunction (StepTy stgy) t
  , PlotValue t
  , PlotValue ohlc
  , ToYield ohlc
  , Pretty ohlc
  , Pretty (DeltaTy t)
  , Real (DeltaTy t)
  , Pretty (Stats.DeltaTyStats t)
  , Line.ToLine (Vec.Vector (t, ohlc))
  , Line.XTy (Vec.Vector (t, ohlc)) ~ t
  , Line.YTy (Vec.Vector (t, ohlc)) ~ ohlc) =>
  String -> String -> Result stgy sym t ohlc -> HtmlIO
render symTitle btTitle (Result inp out) = do
  
  Rep.subheader "Experiment"

  let ts = Vec.map fst (unSignal (snd (head (inputSignals inp))))

  SRep.plot (impulseSignals out) (alignedSignals out)
  
  Rep.backtestChart
    (Rep.gridChart (Style.axTitle "Equity" "XXX")
      [ Line symTitle (snd (head (inputSignals inp))) ])
--      , Line btTitle (outputSignal out)])
    (Rep.impulseSignalCharts []) -- curve ts (snd (head (impulseSignals out)))])

  Rep.subsubheader "Summary"

  toHtmlIO (SS.sampleStatistics (snd (head (inputSignals inp))))
  toHtmlIO (SS.sampleStatistics (outputSignal out))

  Rep.subsubheader "Trade statistics"

  tradeStatistics (step inp) (deltaTradeList out)
