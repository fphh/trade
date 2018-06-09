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

import Trade.Type.EquityAndShare
import Trade.Type.Yield

import Trade.Timeseries.Quandl.Database (Symbol)
import Trade.Timeseries.Url (ToUrl, toUrl)
-- import Trade.Timeseries.Row (RowInterface, closeR)
import Trade.Timeseries.OHLC

import Trade.Render.Svg.Plot

import Trade.Trade.ImpulseSignal
import Trade.Trade.PriceSignal
import Trade.Trade.TradeList

import qualified Trade.Analysis.Broom as Broom
import Trade.Analysis.Broom (Broom)
import Trade.Analysis.NormHistory
import Trade.Analysis.Backtest


import qualified Trade.Report.Report as Report

import Debug.Trace

newtype Fraction = Fraction {
  unFraction :: Double
  } deriving (Show)


data ReportInput sym ohlc trdAt = ReportInput {
  title :: String
  , symbol :: sym
  , begin :: UTCTime
  , end :: UTCTime
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
      
  broom <- Broom.normHistoryBroom (monteCarloN args) (begin args) (end args) ntrades

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
            
      -- tickerLine = Report.lineL (toUrl (symbol inArgs)) (Vec.map (fmap unClose) (unPriceSignal (priceSignal outArgs)))
      -- tickerLine2 = Report.line (toUrl (symbol inArgs)) (Vec.map (fmap unClose) (unPriceSignal (priceSignal outArgs)))

      bt = backtest (tradeAt inArgs) (Equity 100000) (tradeList outArgs)

      intersArgs = ImpulseArgs 0.2 0.1
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
        -- (Report.svgLR [ cdl {- tickerLine, bt -} ] [ inters ])
        -- : (Report.svg [ tickerLine2 ])

        
        -- :
        Report.candle (toUrl (symbol inArgs)) [toCandle (priceSignal outArgs)]
        : (Report.subheader "Monte Carlo Sample")
        : (Report.svg (Broom.broom2chart n normEqBroom))


        : (Report.subheader "Terminal Wealth Relative")
        : (Report.svg ((map twr2line twrs)++ [baseLineTWR]))


        : (Report.subheader "Probability of ending with less than 100% of initial equity")
        : (Report.vtable (map twr2row twrs))

        : (Report.subheader "CAR /01 /25 /50 /75 /100")
        : (Report.vtable (map (twr2car [0.01, 0.25, 0.5, 0.75, 1.0]) twrs))
          

        : (Report.subheader "Risk Analysis")
        : (Report.svg (map toRisk twrs ++ [baseLineRisk, baseLineRisk2]))

        : []

      rep = Report.report (Report.header (title inArgs) : stats ++ items)

  in Report.renderReport rep


renderStats :: [Report sym ohlc trdAt] -> IO BSL.ByteString
renderStats rs =
  let f r =
        Report.subheader (title (reportInput r))
        : (map TS.stats2para (tradeStatistics (reportOutput r)))

      ss = concatMap f rs

      rep = Report.report $
        Report.header "Stats"
        : ss

  in Report.renderReport rep
