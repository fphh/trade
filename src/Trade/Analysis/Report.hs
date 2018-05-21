{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.Report where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.Printf (printf)


import qualified Trade.TStatistics.TradeStatistics as TS

import Trade.Type.EquityAndShare
import Trade.Type.Yield

import Trade.Timeseries.Quandl.Database (Symbol, Dataset, toUrl)

import Trade.Render.Svg.Plot

import Trade.Trade.ImpulseSignal
import Trade.Trade.PriceSignal
import Trade.Trade.TradeList

import qualified Trade.Analysis.Broom as Broom
import Trade.Analysis.Broom (Broom)
import Trade.Analysis.NormHistory
import Trade.Analysis.Backtest


import qualified Trade.Report.Report as Report

newtype Fraction = Fraction {
  unFraction :: Double
  } deriving (Show)


data ReportInput ex ohlc = ReportInput {
  title :: String
  , symbol :: Symbol ex
  , begin :: UTCTime
  , end :: UTCTime
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

data Report ex ohlc = Report {
  reportInput :: ReportInput ex ohlc
  , reportOutput :: ReportOutput ohlc
  }


  
prepareReport ::
  Vector (UTCTime, Double) -> ReportInput ex Close -> IO (Report ex Close)
prepareReport qsTsDateClose' args = do
  let qsTsDateClose = PriceSignal (Vec.map (fmap Close) qsTsDateClose')
      impulses = (generateImpulses args) qsTsDateClose
      trades = impulses2trades qsTsDateClose impulses
      ntrades = trades2normTrades trades
      
  broom <- Broom.normHistoryBroom (monteCarloN args) (begin args) (end args) ntrades

  let toTWR frac =
        let br = Broom.normEquityBroom ((step args) frac) (initialEquity args) broom
        in (frac, Broom.terminalWealthRelative (initialEquity args) br, Broom.risk br)

      twrs = map toTWR (fractions args)

      stats = TS.tradeStatistics trades
      
  return $ Report {
    reportInput = args
    , reportOutput = ReportOutput {
        priceSignal = qsTsDateClose
        , impulseSignal = impulses
        , tradeList = trades
        , broom = broom
        , terminalWealthRelative = twrs
        , tradeStatistics = stats
        }
    }

renderReport :: (Show ex, Show (Dataset ex)) => Report ex Close -> BSL.ByteString
renderReport report =
  let inArgs = reportInput report
      outArgs = reportOutput report
      
      tickerLine = Line (toUrl (symbol inArgs)) (Vec.map (fmap unClose) (unPriceSignal (priceSignal outArgs)))

      intersArgs = ImpulseArgs 25 25
      inters = impulse2line intersArgs (impulseSignal outArgs)

      normEqBroom = Broom.normEquityBroom ((step inArgs) (Fraction 1)) (initialEquity inArgs) (broom outArgs)

      twrs = terminalWealthRelative outArgs

      n = 10
      baseLineTWR = Line "1.0" (Vec.fromList [(0, 1), (1, 1)])
      baseLineRisk = Line "95%" (Vec.fromList [(0.95, 0), (0.95, 1)])
      baseLineRisk2 = Line "20%" (Vec.fromList [(0, 0.2), (1, 0.2)])

      twr2line (frac, twr, _) = Line (printf "Frac %.2f" (unFraction frac)) twr

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
        in Line label risk
     
      stats = map TS.stats2para (tradeStatistics outArgs)

      rep = Report.report $
        (Report.header (title inArgs))

        : stats ++

        ((Report.svg [ tickerLine, inters, backtest (Equity 100) (tradeList outArgs) ])

        -- : (broom2chart n (broom outArgs))
        
        : (Report.subheader "Monte Carlo Sample")
        : (Broom.broom2chart n normEqBroom)

        : (Report.subheader "Terminal Wealth Relative")
        : (Report.svg ((map twr2line twrs) ++ [baseLineTWR]))


        : (Report.subheader "Probability of ending with less than 100% of initial equity")
        : (Report.vtable (map twr2row twrs))

        : (Report.subheader "CAR /01 /25 /50 /75 /100")
        : (Report.vtable (map (twr2car [0.01, 0.25, 0.5, 0.75, 1.0]) twrs))
          

        : (Report.subheader "Risk Analysis")
        : (Report.svg (map toRisk twrs ++ [baseLineRisk, baseLineRisk2]))

        : [])
        
  in Report.renderReport rep


renderStats :: (Show ex, Show (Dataset ex)) => [Report ex Close] -> BSL.ByteString
renderStats rs =
  let f r =
        Report.subheader (title (reportInput r))
        : (map TS.stats2para (tradeStatistics (reportOutput r)))

      ss = concatMap f rs

      rep = Report.report $
        Report.header "Stats"
        : ss

  in Report.renderReport rep
