{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Trade.Timeseries.Quandl.Database where


import Data.Time.Clock (UTCTime)
-- import Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)


import Trade.Timeseries.Url

data Symbol dbCode = Symbol {
  dbCode :: dbCode
  , dsCode :: Dataset dbCode
  }


deriving instance (Eq dbCode, Eq (Dataset dbCode)) => Eq (Symbol dbCode)

deriving instance (Ord dbCode, Ord (Dataset dbCode)) => Ord (Symbol dbCode)

deriving instance (Show dbCode, Show (Dataset dbCode)) => Show (Symbol dbCode)

deriving instance (Read dbCode, Read (Dataset dbCode)) => Read (Symbol dbCode)


instance (Show dbCode, Show (Dataset dbCode)) => ToUrl (Symbol dbCode) where
  toUrl (Symbol x y) = "/" ++ show x ++ "/" ++ show y

class FromDatabase dbCode where
  type Dataset dbCode :: *

  fromDatabase :: dbCode -> Dataset dbCode -> Symbol dbCode
  fromDatabase = Symbol 


data RequestParams dbCode = RequestParams {
  baseUrl :: String
  , symbol :: Symbol dbCode
  , apiKey :: String
  , from :: Maybe UTCTime
  , to :: Maybe UTCTime
  }


instance (Show dbCode, Show (Dataset dbCode)) => ToUrl (RequestParams dbCode) where
  toUrl RequestParams{..} =
    baseUrl ++ toUrl symbol ++ ".csv?"
    ++ maybe "" (\d -> "start_date=" ++ show d ++ "&") from
    ++ maybe "" (\d -> "end_date=" ++ show d ++ "&") to
    ++ "api_key=" ++ apiKey

class ToRow row where
  type RowTy row :: *
    
