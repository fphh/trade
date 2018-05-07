{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Trade.Type.EquityAndShare where

import Trade.Report.Pretty


import qualified Data.ByteString.Char8 as BS

import Data.Csv (FromField, parseField)


newtype Equity = Equity { unEquity :: Double } deriving (Show, Eq, Ord, Num)

instance Pretty Equity where
  pretty (Equity x) = "Equity=" ++ show x


newtype Open = Open { unOpen :: Double } deriving (Show, Eq, Ord)

instance FromField Open where
  parseField =  return . Open . read . BS.unpack

instance Pretty Open where
  pretty = show

newtype Close = Close { unClose :: Double } deriving (Show, Eq, Ord)

instance FromField Close where
  parseField =  return . Close . read . BS.unpack

instance Pretty Close where
  pretty (Close x) = "Close=" ++ show x

newtype High = High { unHigh :: Double } deriving (Show, Eq, Ord)

instance FromField High where
  parseField =  return . High . read . BS.unpack

instance Pretty High where
  pretty = show

newtype Low = Low { unLow :: Double } deriving (Show, Eq, Ord)

instance FromField Low where
  parseField =  return . Low . read . BS.unpack

instance Pretty Low where
  pretty = show

newtype Volume = Volume { unVolume :: Integer } deriving (Show, Eq, Ord, Num)

instance FromField Volume where
  parseField =  return . Volume . read . BS.unpack

instance Pretty Volume where
  pretty = show


newtype OutToInRatio ohcl = OutToInRatio { unOutToInRatio :: Double } deriving (Show)

class ToRatio ohcl where
  (./) :: ohcl -> ohcl -> OutToInRatio ohcl

instance ToRatio Close where
  Close x ./ Close y = OutToInRatio (x/y)


newtype Share = Share { unShare :: Integer } deriving (Show, Eq, Ord, Num)

instance Pretty Share where
  pretty (Share s) = "Share=" ++ show s

class Mult ohcl where
  mult :: Share -> ohcl -> Equity

instance Mult Open where
  mult (Share s) (Open c) = Equity (c * fromIntegral s)

instance Mult Close where
  mult (Share s) (Close c) = Equity (c * fromIntegral s)

instance Mult Low where
  mult (Share s) (Low c) = Equity (c * fromIntegral s)

instance Mult High where
  mult (Share s) (High c) = Equity (c * fromIntegral s)



class Div ohcl where
  div :: Equity -> ohcl -> Share

instance Div Open where
  div (Equity e) (Open pps) = Share (floor (e/pps))

instance Div Close where
  div (Equity e) (Close pps) = Share (floor (e/pps))

instance Div Low where
  div (Equity e) (Low pps) = Share (floor (e/pps))

instance Div High where
  div (Equity e) (High pps) = Share (floor (e/pps))

totalEquity :: Equity -> Share -> Close -> Equity
totalEquity e s pps = e + s `mult` pps

