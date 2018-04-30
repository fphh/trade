{-# LANGUAGE FlexibleContexts #-}


module Trade.Quandl.Quandl where

import Control.Monad (foldM)

import Network.HTTP.Simple

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Csv as Csv

import Trade.Quandl.Quandl.Database

quandlBaseUrl :: String
quandlBaseUrl = "https://www.quandl.com/api/v3/datasets"

getUnsafe ::
  (Show dbCode, Show (Dataset dbCode), ToRow dbCode, Csv.FromNamedRecord (RowTy dbCode))
  => RequestParams dbCode -> IO (Vector (RowTy dbCode))
getUnsafe request = do
  initReq <- parseRequest (toUrl request)
  txt <- httpLBS initReq
  case Csv.decodeByName (getResponseBody txt) of
    Left str -> error str
    Right (_, v) -> return v

{-
getTicker :: (Show dbCode, Show (Dataset dbCode), ToRow dbCode, Csv.FromNamedRecord (RowTy dbCode)) => Symbol dbCode -> IO (Vector (RowTy dbCode))
getTicker sym = do
  r0 <- newRequest sym
  fmap Vec.reverse (getUnsafe r0)
-}

getTicker :: (Show dbCode, Show (Dataset dbCode), ToRow dbCode, Csv.FromNamedRecord (RowTy dbCode)) => (Symbol dbCode -> RequestParams dbCode) -> Symbol dbCode -> IO (Vector (RowTy dbCode))
getTicker req sym = fmap Vec.reverse (getUnsafe (req sym))

getTickers :: (Show dbCode, Show (Dataset dbCode), ToRow dbCode, Csv.FromNamedRecord (RowTy dbCode), Ord dbCode, Ord (Dataset dbCode)) => (Symbol dbCode -> RequestParams dbCode) -> [Symbol dbCode] -> IO (Map (Symbol dbCode) (Vector (RowTy dbCode)))
getTickers req qs =
  let buildMap acc sym = do
        ts <- getTicker req sym
        return (Map.insert sym ts acc)
  in foldM buildMap Map.empty qs
