{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.Type.OHLC where

import Trade.Type.Equity (Equity(..))

import Trade.Report.Pretty (Pretty, pretty)

import qualified Data.ByteString.Char8 as BS

import Data.Csv (FromField, parseField)

-- | Open.
newtype Open = Open {
  unOpen :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField Open where
  parseField =  return . Open . read . BS.unpack

instance Pretty Open where
  pretty = show

-- | Close.
newtype Close = Close {
  unClose :: Double
  } deriving (Show, Read, Eq, Ord, Num)

data CloseUnit

instance FromField Close where
  parseField =  return . Close . read . BS.unpack

instance Pretty Close where
  pretty (Close x) = "Close=" ++ show x

-- | High.
newtype High = High {
  unHigh :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField High where
  parseField =  return . High . read . BS.unpack

instance Pretty High where
  pretty = show

-- | Low.
newtype Low = Low {
  unLow :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField Low where
  parseField =  return . Low . read . BS.unpack

instance Pretty Low where
  pretty = show

-- | Volume.
newtype Volume = Volume {
  unVolume :: Double
  } deriving (Show, Read, Eq, Ord, Num)

instance FromField Volume where
  parseField =  return . Volume . read . BS.unpack

instance Pretty Volume where
  pretty = show
