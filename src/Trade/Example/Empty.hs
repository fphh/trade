
module Trade.Example.Empty where


import qualified Data.ByteString.Lazy.Char8 as BSL

import Trade.Type.ImpulseGenerator (noImpulses)

import Trade.Report.Config (HtmlReader)
import Trade.Report.HtmlReader (render)

import Trade.Analysis.Analysis (Analysis(..), analyzeHelper)
import Trade.Analysis.Optimize (NoOptimization(..), emptyOptimize)
import Trade.Analysis.Backtest (NoBacktest(..), noBacktest)


analyze :: Analysis NoOptimization b optInpTy ohlc -> HtmlReader ()
analyze = analyzeHelper emptyOptimize noBacktest

example :: IO ()
example = do

  let analysis :: Analysis NoOptimization NoBacktest () a
      analysis = Analysis {
        title = "Empty Report"
        , impulseGenerator = noImpulses
        , optimizationInput = NoOptimization
        , backtestInput = NoBacktest
        }

      rep = analyze analysis

  t <- render rep
  
  BSL.putStrLn t
  
