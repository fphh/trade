{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Trade.Type.OHLC where

import Trade.Type.Conversion.Type2Double (Type2Double, type2double)

import Trade.Report.Pretty (Pretty, pretty)

import Trade.Type.Delta (Delta(..), CDelta, DeltaType, DeltaTy, Add, add, diff)
import Trade.Type.Scale (Scale, scale, factor)


import qualified Data.ByteString.Char8 as BS

import Data.Csv (FromField, parseField)

-- | Open.
newtype Open = Open {
  unOpen :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField Open where
  parseField =  return . Open . read . BS.unpack

instance Type2Double Open where
  type2double (Open x) = x

instance Pretty Open where
  pretty = show

-- | Close.
newtype Close = Close {
  unClose :: Double
  } deriving (Show, Read, Eq, Ord, Num)

instance FromField Close where
  parseField =  return . Close . read . BS.unpack

instance Type2Double Close where
  type2double (Close x) = x

instance Pretty Close where
  pretty (Close x) = "Close=" ++ show x

instance DeltaType Close where
  type DeltaTy Close = CDelta Close

instance Add Close where
  add (Delta dx) (Close x) = Close (x+dx)
  diff (Close p) (Close q) = Delta (p-q)

instance Scale Close where
  factor q (Close x) = q/x
  scale q (Close x) = Close (q*x)

{-
instance Scale (Delta Close) where
  factor q (Delta dx) = q/dx
  scale q (Delta dx) = Delta (q*dx)
-}

-- | High.
newtype High = High {
  unHigh :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField High where
  parseField =  return . High . read . BS.unpack

instance Type2Double High where
  type2double (High x) = x

instance Pretty High where
  pretty = show

-- | Low.
newtype Low = Low {
  unLow :: Double
  } deriving (Show, Read, Eq, Ord)

instance FromField Low where
  parseField =  return . Low . read . BS.unpack

instance Type2Double Low where
  type2double (Low x) = x

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
