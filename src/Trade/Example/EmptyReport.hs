
module Trade.Example.EmptyReport where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Trade.Report.Report as Report

import Trade.Type.ImpulseGenerator (noImpulses)
import Trade.Analysis.Analysis (Analysis(..), analyze)
import Trade.Analysis.Optimize (NoOptimization(..))
import Trade.Analysis.Backtest (NoBacktest(..))



emptyReport :: IO ()
emptyReport = do

  let analysis = Analysis {
        impulseGenerator = noImpulses
        , optimizationInput = NoOptimization
        , backtestInput = NoBacktest
        }

      rep = analyze analysis

  t <- Report.renderReport (Report.report rep)
  
  BSL.putStrLn t
  
