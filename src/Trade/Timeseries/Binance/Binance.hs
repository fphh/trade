

module Trade.Timeseries.Binance.Binance where


import Control.Monad (foldM)

import qualified Data.Vector as Vec
import Data.Vector (Vector)
  
import qualified Data.Map as Map
import Data.Map (Map)

import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)

import Trade.Timeseries.Binance.Database (RequestParams, limit, to)
import Trade.Timeseries.Binance.Symbol (Row, fromDate)
import Trade.Timeseries.Url (toUrl)



binanceBaseUrl :: String
binanceBaseUrl = "https://api.binance.com/api/v1/klines"

getUnsafe :: RequestParams -> IO (Vector Row)
getUnsafe request = do
  initReq <- parseRequest (toUrl request)
  fmap getResponseBody (httpJSON initReq)

getTicker :: RequestParams -> IO (Vector Row)
getTicker req =
  case limit req of
    Nothing -> getUnsafe req
    Just x | x <= 1000 -> getUnsafe req
    Just x -> do
      v <- getUnsafe req
      let newReq = req {
            limit = Just (x - 1000)
            , to = Just (fromDate (Vec.head v))
            }
      vs <- getTicker newReq
      return (vs Vec.++ v)


getTickers :: (Ord k, Foldable t) => (k -> RequestParams) -> t k -> IO (Map k (Vector Row))
getTickers req qs =
  let buildMap acc sym = do
        ts <- getTicker (req sym)
        return (Map.insert sym ts acc)
  in foldM buildMap Map.empty qs
