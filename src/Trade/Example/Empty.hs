
module Trade.Example.Empty where


import qualified Data.ByteString.Lazy.Char8 as BSL

import Trade.Report.HtmlReader (render)

import Trade.Analysis.Analysis (Analysis(..), analyze)
import Trade.Analysis.Optimize (NoOptimization(..))
import Trade.Analysis.Backtest (NoBacktest(..))



example :: IO ()
example = do

  let analysis :: Analysis NoOptimization NoBacktest
      analysis = Analysis {
        title = "Empty Report"
        , impulseGenerator = undefined -- optImpGen2impGen noImpulses
        , optimizationInput = NoOptimization
        , backtestInput = NoBacktest
        }

      rep = analyze analysis

  t <- render rep
  
  BSL.putStrLn t
  
