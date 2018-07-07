{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trade.Type.OHLC where

import Trade.Report.Pretty


import qualified Data.ByteString.Char8 as BS

import Data.Csv (FromField, parseField)

class UnOHLC a where
  unOHLC :: a -> Double

-- | Open.
newtype Open = Open {
  unOpen :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField Open where
  parseField =  return . Open . read . BS.unpack

instance UnOHLC Open where
  unOHLC (Open x) = x

instance Pretty Open where
  pretty = show

-- | Close.
newtype Close = Close {
  unClose :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField Close where
  parseField =  return . Close . read . BS.unpack

instance UnOHLC Close where
  unOHLC (Close x) = x

instance Pretty Close where
  pretty (Close x) = "Close=" ++ show x

-- | High.
newtype High = High {
  unHigh :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField High where
  parseField =  return . High . read . BS.unpack

instance UnOHLC High where
  unOHLC (High x) = x

instance Pretty High where
  pretty = show

-- | Low.
newtype Low = Low {
  unLow :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField Low where
  parseField =  return . Low . read . BS.unpack

instance UnOHLC Low where
  unOHLC (Low x) = x

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
