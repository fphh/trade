

module Trade.Statistics.Summary where

import Control.Applicative (liftA2)

import qualified Data.Vector as Vec

import qualified Statistics.Sample as Sample

import qualified Trade.Type.DeltaSignal.Algorithm as DSA
import Trade.Type.DeltaSignal (DeltaSignal(..))

import Trade.Type.Yield (LogYield(..))

import Trade.Report.Pretty (Pretty, pretty)
import qualified Trade.Report.Table as Table
import Trade.Report.ToReport (ToReport, toReport)



data Summary = Summary {
  totalTrades :: Int
  , profitFactor :: Double
  , avgProfit :: Double
  , stdDevProfit :: Double
  , winners :: Int
  , largestWinner :: Double
  , avgWinner :: Double
  , losers :: Int
  , largestLoser :: Double
  , avgLoser :: Double
  , cumulativeProfit :: Double
  }


summary :: [LogYield ohlc] -> [LogYield ohlc] -> Summary
summary winners losers =
  let ws = Vec.fromList (map logYield winners)
      ls = Vec.fromList (map logYield losers)
  in Summary {
    totalTrades = length winners + length losers
    , profitFactor = exp (sum ws) / exp (sum ls)
    , avgProfit = exp (Sample.mean (Vec.concat [ws, ls]))
    , stdDevProfit = exp (Sample.stdDev ws)

    , winners = Vec.length ws
    , largestWinner = exp (Vec.maximum ws)
    , avgWinner = exp (Sample.mean ws)
    
    , losers = Vec.length ls
    , largestLoser = exp (Vec.minimum ls)
    , avgLoser = exp (Sample.mean ls)

    , cumulativeProfit = exp (sum ws + sum ls)
    }


summary2table :: Summary -> [[String]]
summary2table ys =
  [ [ "Summary" ]
  , []
  , [ "Total trades", pretty (totalTrades ys) ]
  , [ "Profit factor", pretty (profitFactor ys) ]
  , [ "Avg. profit", pretty (avgProfit ys) ]
  , [ "StdDev. profit", pretty (stdDevProfit ys) ]
  , []
  , [ "Winners", pretty (winners ys) ]
  , [ "Largest winner", pretty (largestWinner ys) ]
  , [ "Avg. winner", pretty (avgWinner ys) ]
  , []
  , [ "Losers", pretty (losers ys) ]
  , [ "Largest loser", pretty (largestLoser ys) ]
  , [ "Avg. loser", pretty (avgLoser ys) ]
  , []
  , [ "Cumulativ profit", pretty (cumulativeProfit ys) ]
  ]



instance ToReport Summary where
  toReport = Table.table . summary2table



toSummary ::
  (Applicative f) =>
  f [DeltaSignal ohlc] -> f [DeltaSignal ohlc] -> f Summary
toSummary xs ys = liftA2 summary (fmap (map DSA.yield) xs) (fmap (map DSA.yield) ys)


