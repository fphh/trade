

module Trade.Example.Correlation where

import Control.Applicative (liftA2)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Time.Clock (UTCTime, getCurrentTime)

import qualified Statistics.Sample as Sample

import Trade.Type.Bars (BarLength(..))

import Trade.Type.OHLC (Close(..))

import qualified Trade.Timeseries.Binance.Binance as Bin
import qualified Trade.Timeseries.Binance.Binance as Bin
import qualified Trade.Timeseries.Binance.Database as Bin
import qualified Trade.Timeseries.Binance.Interval as Bin
import qualified Trade.Timeseries.Binance.Symbol as Bin

import qualified Trade.Report.Heatmap as Heat
import qualified Trade.Report.Report as Rep


barLength :: BarLength
barLength = Min 1


ts :: [Bin.Symbol]
-- ts = map Bin.ETH [ Bin.ADAETH .. Bin.ZRXETH]
ts = map Bin.ETH [ Bin.BNBETH, Bin.ENJETH, Bin.HOTETH, Bin.FUELETH, Bin.XRPETH, Bin.ADAETH, Bin.EOSETH, Bin.VETETH, Bin.TRXETH, Bin.DENTETH]

getSymbols :: [Bin.Symbol] -> IO (Map Bin.Symbol (Vector Double))
getSymbols ts = do

  now <- getCurrentTime
  
  let req sym = Bin.RequestParams {
        Bin.baseUrl = Bin.binanceBaseUrl
        , Bin.symbol = sym
        , Bin.interval = Bin.Interval barLength
        , Bin.limit = Just 1000
        , Bin.from = Nothing
        , Bin.to = Just now
        }

      toSignal row = (Bin.toDate row, unClose (Bin.close row))

  tickers <- fmap (fmap (Vec.map toSignal)) (Bin.getTickers req ts)

  let
      tmsTickers = fmap (Set.fromList . Vec.toList . Vec.map fst) tickers
      allTimes = foldMap id tmsTickers
      tms = Map.foldr (\vs acc -> Set.intersection acc vs) allTimes tmsTickers

      res = fmap (Vec.map snd . Vec.filter ((`Set.member` tms) . fst)) tickers

 -- error (show res)
  return res


example :: IO ()
example = do
  ms <- getSymbols ts

  let xs = Map.toList ms
      f (Bin.ETH u, us) (Bin.ETH v, vs) = ((u, v), Sample.correlation (Vec.zip us vs))
      as = filter (\((x, y), _) -> x /= y) (liftA2 f xs xs)
      
  t <- Rep.renderReport (Heat.heatmap 0 (Map.fromList as))
  
  BSL.putStrLn t
 
