{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Trade.Analysis.Report where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.Printf (printf)

import qualified Graphics.Rendering.Chart.Easy as E

import qualified Trade.TStatistics.TradeStatistics as TS

import Trade.Type.Equity ()
import Trade.Type.Yield ()
import Trade.Type.Bars ()
import Trade.Type.History
import Trade.Type.Broom (Broom)
import Trade.Type.Signal ()
import Trade.Type.Signal.Price ()
import Trade.Type.Signal.Impulse ()

import Trade.Timeseries.Quandl.Database (Symbol)
import Trade.Timeseries.Url (ToUrl, toUrl)
-- import Trade.Timeseries.Row (RowInterface, closeR)
import Trade.Timeseries.OHLC

import Trade.Render.Svg.Plot

import Trade.Trade.TradeList

import qualified Trade.Analysis.Broom as Broom
import Trade.Analysis.Backtest
import qualified Trade.MonteCarlo.ResampleTrades.MonteCarlo as MC


import qualified Trade.Report.Report as Report

import Debug.Trace

{-

newtype Fraction = Fraction {
  unFraction :: Double
  } deriving (Show)


data ReportInput sym ohlc trdAt = ReportInput {
  title :: String
  , description :: String
  , symbol :: sym
  -- , simStart :: UTCTime
  , simBars :: Bars
  -- , end :: UTCTime
  , tradeAt :: ohlc -> trdAt
  , initialEquity :: Equity
  , monteCarloN :: Int
  , generateImpulses :: PriceSignal ohlc -> ImpulseSignal ohlc
  , step :: Fraction -> Equity -> Yield -> Equity
  , fractions :: [Fraction]
  }


data ReportOutput ohlc = ReportOutput {
  priceSignal :: PriceSignal ohlc
  , impulseSignal :: ImpulseSignal ohlc
  , tradeList :: TradeList ohlc
  , broom :: Broom (NormHistory ohlc)
  , terminalWealthRelative :: [(Fraction, Vector (Broom.Percent, Double), Vector (Broom.Percent, Double))]
  , tradeStatistics :: [TS.TradeStatistics]
  }

data Report ex ohlc trdAt = Report {
  reportInput :: ReportInput ex ohlc trdAt
  , reportOutput :: ReportOutput ohlc
  }

  
prepareReport ::
  (OHLCInterface ohlc, ToYield ohlc) =>
  PriceSignal ohlc -> ReportInput ex ohlc trdAt -> IO (Report ex ohlc trdAt)
prepareReport qsTs args = do
  let impulses = (generateImpulses args) qsTs
      trades = impulses2trades qsTs impulses
      ntrades = trades2normTrades trades
      
  broom <- Broom.normHistoryBroom (simBars args) (monteCarloN args) ntrades

  let toTWR frac =
        let br = Broom.normEquityBroom ((step args) frac) (initialEquity args) broom
        in (frac, Broom.terminalWealthRelative (initialEquity args) br, Broom.risk br)

      twrs = map toTWR (fractions args)
  
      stats = TS.tradeStatistics ohlcClose trades
      
  return $ Report {
    reportInput = args
    , reportOutput = ReportOutput {
        priceSignal = qsTs
        , impulseSignal = impulses
        , tradeList = trades
        , broom = broom
        , terminalWealthRelative = twrs
        , tradeStatistics = stats
        }
    }

renderExtendedReport :: (ToUrl ex, OHLCInterface ohlc, UnOHLC trdAt) => Report ex ohlc trdAt -> IO BSL.ByteString
renderExtendedReport report =
  let inArgs = reportInput report
      outArgs = reportOutput report

      toC (t, ohlc) =
        let c = E.Candle t
                (unOHLC $ ohlcLow ohlc)
                (unOHLC $ ohlcOpen ohlc)
                0
                (unOHLC $ ohlcClose ohlc)
                (unOHLC $ ohlcHigh ohlc)
        in c
      toCandle (PriceSignal ps) = Vec.map toC ps

      initEq = Equity 100000

  
      (_, firstPrice) = Vec.head (unPriceSignal (priceSignal outArgs))
      fp = unEquity initEq / (unOHLC (tradeAt inArgs firstPrice))
      
      tickerLine =
        Report.lineL
        (toUrl (symbol inArgs))
        (Vec.map (fmap ((fp *) . unOHLC . tradeAt inArgs)) (unPriceSignal (priceSignal outArgs)))

      bt = backtest (tradeAt inArgs) initEq (tradeList outArgs)

      -- impulseAxisConf ::(E.PlotValue y, RealFloat y, Show y, Num y) =>  AxisConfig y
      impulseAxisConf =
        let al = E.laxis_style E..~ (E.axis_grid_style E..~ (E.line_width E..~ 0 $ E.def) $ E.axis_line_style E..~ (E.line_width E..~ 0 $ E.def) $ E.def)
                 $ E.def
            av = E.axis_show_labels E..~ False
                 $ E.axis_show_ticks E..~ False
                 $ E.def
            af = E.scaledAxis E.def (-1,10)
        in Report.AxisConfig al av (Just af)

      axTitle str =
        let al = E.laxis_title E..~ str $ E.def
        in Report.AxisConfig al E.def Nothing
          
      intersArgs = ImpulseArgs 0 1
      inters = impulse2line intersArgs (impulseSignal outArgs)

      normEqBroom = Broom.normEquityBroom ((step inArgs) (Fraction 1)) (initialEquity inArgs) (broom outArgs)

      twrs = terminalWealthRelative outArgs

      n = 10
      baseLineTWR = Report.line "1.0" [(0, 1), (1, 1)]
      baseLineRisk = Report.line "95%" [(0.95, 0), (0.95, 1)]
      baseLineRisk2 = Report.line "20%" [(0, 0.2), (1, 0.2)]

      twr2line (frac, twr, _) = Report.line (printf "Frac %.2f" (unFraction frac)) twr

      twr2row (frac, twr, _) =
        printf "Frac %.2f" (unFraction frac)
        : case Vec.find ((>1) . snd) twr of
            Just (x, _) -> printf "%.2f%%" (100*x)
            Nothing -> "n/a"
        : []

      twr2car ds (frac, twr, _) =
        printf "Frac %.2f" (unFraction frac)
        : (map (\d -> let idx = round (fromIntegral (Vec.length twr) * d) - 1
                      in printf "%.2f%%" (100 * snd (twr Vec.! idx))) ds)
        
      toRisk (frac, _, risk) =
        let label = printf "Frac %.2f" (unFraction frac)
        in Report.line label risk
     
      [pos, nopos] = map TS.stats2para (tradeStatistics outArgs)

      items =
        Report.header (title inArgs)
        : Report.text (description inArgs)

        : Report.subheader "Historical Ticker"
        : Report.text "This is just the ticker with open/high/low/close candle sticks"
        : Report.candle (toUrl (symbol inArgs)) [toCandle (priceSignal outArgs)]

        : Report.subheader "Strategy applied"
        : Report.text "The strategy generates Buy/Sell signals (green line: down = buy, up = sell). The strategy is then applied to the ticker (red line) to see, how it would have performed on historical data (blue line). The blue line is called \"backtest\". This would be the equity curve if this strategy had be traded for real in the past."
        : (Report.svgLR (axTitle "Time") (axTitle "Equity", [tickerLine, bt ]) (impulseAxisConf, [inters]))

        : (Report.subheader "Trade Statistics")
        : Report.text "Using the green line from above, we cut the red line into pieces. This gives us periods of time where we hold a position, and periods where we do not hold any position. Here are the trade statistics:"
        : pos
        : nopos
        : Report.text "The yield is logarithmic, that is eq_out = eq_in * exp(yield). Ideally, the yield should be positive, when we hold a position and negative, when we do not hold any position. Negative yield, when not holding any position, means \"buy and hold\" would be a better strategy than ours."
        
        : (Report.subheader "Monte Carlo Sample")
        : Report.text "From the trade list, we generate several thounds of trading histories, choosing alternatingly Long and NoPosition trades at random. We set the simulated period to 2 years, for example. Here is a broom chart with 10 of those histories:"
        : (Report.svg (axTitle "Time") (axTitle "Equity", Broom.broom2chart n normEqBroom))
        : Report.text "We used these trading histories to generate two distributions, 1. of terminal wealth, and 2. of drawdown experienced during the period simulated."

        : Report.subheader "Fraction"
        : Report.text "When trading live, we can choose per trade how much of our equity we want to invest. For example, we can choose for each trade to only use 40% of our funding and to let 60% unused in our account. This has an impact on profits/losses/risk. Of course, we can also use for example 2 times of our funding, meaning that we take 1 part from our account and borrow 1 part from someone else. Thus, we can simulate the strategy using leverage. The part used for trading is called \"fraction\"."
        
        : (Report.subheader "Terminal Wealth Relative")
        : Report.text "TWR means whe normalize our profits/losses with regard to our initial equity, which is 1. The chart below shows the final equities sorted ascendingly. The x-axis shows the \"number of history in sorted array / length of array\". That is, we can interpret it as percent of histories having lower yield. For example, trading at a fraction of 80% (yellow line), we have after 2 years less than 1.5 times the initial equity with a probability of around 95%. Conversely, with a probability of 5% we will have more than 1.5 times the initial equity after trading the strategy for more than 2 years."
        : (Report.svg (axTitle "Probability") (axTitle "Times initial Equity", (map twr2line twrs) ++ [baseLineTWR]))


        : (Report.subheader "Probability of ending with less than 100% of initial equity")
        : Report.text "Those are the probabilities where the line for each fraction crosses TWR 1, e.g. the probability of finishing after 2 years with less than your initial equity."
        : (Report.vtable (map twr2row twrs))

        : (Report.subheader "Risk Analysis")
        : Report.text "The question we want to answer is: At what maximal fraction can we trade if, over a period of 2 years, the maximum drawdown experienced should be less than 20% with probability 95%? For this, we calculate the maximum drawdown for every history in our Monte Carlo Simulation, and sort it ascendingly. We then draw a horizontal line at 20% (green) and a vertical line at 95%. Where those lines cross, the fraction line crossing this point is the maximum fraction still respecting our risk target. For our example, this fraction will be at about 42%." 
        : (Report.svg (axTitle "Probability") (axTitle "% Drawdown", map toRisk twrs ++ [baseLineRisk, baseLineRisk2]))
        
        : (Report.subheader "CAR /01 /25 /50 /75 /100")
        : Report.text "Optimizing our profit, we should strive for a maximum compound annual rate (CAR). For example, with the risk choosen, we can only trade at a fraction about 42%. If we look at the CAR25 row at 40% we get 101.65% of initial equity, wich means that we will have a profit of less than 1.65% per year with probability 25% and a profit grater than 1.65% with 75% probability. Another example: If we trade at fraction 1.2, we will have about 118% to 119% of initial equity per year. However, we will experience a drawdown of 20% or greater with probability 100%-55% = 45% (see chart about risk above, where the black 1.2 fraction line crosses the green horizontal line). With probability 5% we will even experience drawdowns of 50% and greater. Due to randomness in the Monte Carlo simulation, actual values can differ a little bit."
        : (Report.vtable (map (twr2car [0.01, 0.25, 0.5, 0.75, 1.0]) twrs))

        : Report.subheader "Comparing strategies"
        : Report.text "If we compare strategies normalised for risk, we should for example choose CAR25 and then look for the strategy with the highest CAR25. This strategy will have the best \"profit-to-risk\"-ratio."

        : Report.subheader "Disclaimer"
        : Report.text "This is only a prototype which is not validated/verified. Everything is about historical data and has no meaning for the future. You should not try to trade this strategy."
        : []

      -- rep = Report.report (stats ++ items)

  in Report.renderReport (Report.report items)


renderReport :: (ToUrl ex, OHLCInterface ohlc, UnOHLC trdAt) => Report ex ohlc trdAt -> IO BSL.ByteString
renderReport report =
  let inArgs = reportInput report
      outArgs = reportOutput report

      toC (t, ohlc) =
        let c = E.Candle t
                (unOHLC $ ohlcLow ohlc)
                (unOHLC $ ohlcOpen ohlc)
                0
                (unOHLC $ ohlcClose ohlc)
                (unOHLC $ ohlcHigh ohlc)
        in c
      toCandle (PriceSignal ps) = Vec.map toC ps

      initEq = Equity 100000

  
      (_, firstPrice) = Vec.head (unPriceSignal (priceSignal outArgs))
      fp = unEquity initEq / (unOHLC (tradeAt inArgs firstPrice))
      
      tickerLine =
        Report.lineL
        (toUrl (symbol inArgs))
        (Vec.map (fmap ((fp *) . unOHLC . tradeAt inArgs)) (unPriceSignal (priceSignal outArgs)))

      bt = backtest (tradeAt inArgs) initEq (tradeList outArgs)

      -- impulseAxisConf ::(E.PlotValue y, RealFloat y, Show y, Num y) =>  AxisConfig y
      impulseAxisConf =
        let al = E.laxis_style E..~ (E.axis_grid_style E..~ (E.line_width E..~ 0 $ E.def) $ E.axis_line_style E..~ (E.line_width E..~ 0 $ E.def) $ E.def)
                 $ E.def
            av = E.axis_show_labels E..~ False
                 $ E.axis_show_ticks E..~ False
                 $ E.def
            af = E.scaledAxis E.def (-1,10)
        in Report.AxisConfig al av (Just af)

      axTitle str =
        let al = E.laxis_title E..~ str $ E.def
        in Report.AxisConfig al E.def Nothing
          
      intersArgs = ImpulseArgs 0 1
      inters = impulse2line intersArgs (impulseSignal outArgs)

      normEqBroom = Broom.normEquityBroom ((step inArgs) (Fraction 1)) (initialEquity inArgs) (broom outArgs)

      twrs = terminalWealthRelative outArgs

      n = 10
      baseLineTWR = Report.line "1.0" [(0, 1), (1, 1)]
      baseLineRisk = Report.line "95%" [(0.95, 0), (0.95, 1)]
      baseLineRisk2 = Report.line "20%" [(0, 0.2), (1, 0.2)]

      twr2line (frac, twr, _) = Report.line (printf "Frac %.2f" (unFraction frac)) twr

      twr2row (frac, twr, _) =
        printf "Frac %.2f" (unFraction frac)
        : case Vec.find ((>1) . snd) twr of
            Just (x, _) -> printf "%.2f%%" (100*x)
            Nothing -> "n/a"
        : []

      twr2car ds (frac, twr, _) =
        printf "Frac %.2f" (unFraction frac)
        : (map (\d -> let idx = round (fromIntegral (Vec.length twr) * d) - 1
                      in printf "%.2f%%" (100 * snd (twr Vec.! idx))) ds)
        
      toRisk (frac, _, risk) =
        let label = printf "Frac %.2f" (unFraction frac)
        in Report.line label risk
     
      stats = map TS.stats2para (tradeStatistics outArgs)

      items =

        
        Report.candle (toUrl (symbol inArgs)) [toCandle (priceSignal outArgs)]
        : (Report.svgLR (axTitle "Time") (axTitle "Equity", [tickerLine, bt ]) (impulseAxisConf, [inters]))

        : (Report.subheader "Monte Carlo Sample")
        : (Report.svg (axTitle "No. of Bars") (axTitle "Equity", Broom.broom2chart n normEqBroom))

        : (Report.subheader "Terminal Wealth Relative")
        : (Report.svg (axTitle "Probability") (axTitle "Times initial Equity", (map twr2line twrs) ++ [baseLineTWR]))


        : (Report.subheader "Probability of ending with less than 100% of initial equity")
        : (Report.vtable (map twr2row twrs))

        : (Report.subheader "CAR /01 /25 /50 /75 /100")
        : (Report.vtable (map (twr2car [0.01, 0.25, 0.5, 0.75, 1.0]) twrs))
          

        : (Report.subheader "Risk Analysis")
        : (Report.svg (axTitle "Probability") (axTitle "% Drawdown", map toRisk twrs ++ [baseLineRisk, baseLineRisk2]))
        
        : []

      rep = Report.report (Report.header (title inArgs) : stats ++ items)

  in Report.renderReport rep


renderStats :: [Report sym ohlc trdAt] -> IO BSL.ByteString
renderStats rs =
  let f r =
        Report.subheader (title (reportInput r))
        : (Report.text (description (reportInput r)))
        : (map TS.stats2para (tradeStatistics (reportOutput r)))

      ss = concatMap f rs

      rep = Report.report $
        Report.header "Stats"
        : ss

  in Report.renderReport rep

-}
