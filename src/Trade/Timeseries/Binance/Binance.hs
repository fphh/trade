

module Trade.Timeseries.Binance.Binance where


import Control.Monad (foldM)

import Data.Vector (Vector)

import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)

import qualified Data.Map as Map
import Data.Map (Map)

import Trade.Timeseries.Binance.Database (RequestParams)
import Trade.Timeseries.Binance.Symbol (Row)
import Trade.Timeseries.Url (toUrl)



binanceBaseUrl :: String
binanceBaseUrl = "https://api.binance.com/api/v1/klines"

getUnsafe :: RequestParams -> IO (Vector Row)
getUnsafe request = do
  initReq <- parseRequest (toUrl request)
  fmap getResponseBody (httpJSON initReq)

getTicker :: (t -> RequestParams) -> t -> IO (Vector Row)
getTicker req sym = getUnsafe (req sym)

getTickers :: (Ord k, Foldable t) => (k -> RequestParams) -> t k -> IO (Map k (Vector Row))
getTickers req qs =
  let buildMap acc sym = do
        ts <- getTicker req sym
        return (Map.insert sym ts acc)
  in foldM buildMap Map.empty qs
