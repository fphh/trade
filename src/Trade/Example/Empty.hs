
module Trade.Example.Empty where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Trade.Report.Report as Report

import Trade.Type.ImpulseGenerator (noImpulses, optImpGen2impGen)
import Trade.Analysis.Analysis (Analysis(..), analyze)
import Trade.Analysis.Optimize (NoOptimization(..))
import Trade.Analysis.Backtest (NoBacktest(..))



example :: IO ()
example = do

  let analysis = Analysis {
        title = "Empty Report"
        , impulseGenerator = optImpGen2impGen noImpulses
        , optimizationInput = NoOptimization
        , backtestInput = NoBacktest
        }

      rep = analyze analysis

  t <- Report.renderReport rep
  
  BSL.putStrLn t
  
