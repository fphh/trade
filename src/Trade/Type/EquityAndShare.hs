{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trade.Type.EquityAndShare where


import qualified Data.ByteString.Char8 as BS

import Data.Csv (FromField, parseField)


newtype Equity = Equity { unEquity :: Double } deriving (Show, Eq, Ord, Num)


newtype Open = Open { unOpen :: Double } deriving (Show, Eq, Ord)

instance FromField Open where
  parseField =  return . Open . read . BS.unpack


newtype Close = Close { unClose :: Double } deriving (Show, Eq, Ord)

instance FromField Close where
  parseField =  return . Close . read . BS.unpack


newtype High = High { unHigh :: Double } deriving (Show, Eq, Ord)

instance FromField High where
  parseField =  return . High . read . BS.unpack


newtype Low = Low { unLow :: Double } deriving (Show, Eq, Ord)

instance FromField Low where
  parseField =  return . Low . read . BS.unpack


newtype Volume = Volume { unVolume :: Int } deriving (Show, Eq, Ord, Num)

instance FromField Volume where
  parseField =  return . Volume . read . BS.unpack




newtype Share = Share { unShare :: Integer } deriving (Show, Eq, Ord, Num)

newtype Yield = Yield { unYield :: Double } deriving (Show, Eq, Ord, Num)

class EPS a where
  type Ty a :: *
    
  (.*) :: a -> Ty a -> Equity


instance EPS Share where
  type Ty Share = Close

  (Share s) .* (Close pps) = Equity (fromIntegral s * pps)

instance EPS Close where
  type Ty Close = Share 

  (Close pps) .* (Share s) = Equity (fromIntegral s * pps)


sharesPerEquity :: Equity -> Close -> Share
sharesPerEquity (Equity e) (Close pps) = Share (floor (e/pps))

(./) :: Equity -> Close -> Share
(./) = sharesPerEquity

totalEquity :: Equity -> Close -> Share -> Equity
totalEquity e pps s = e + pps .* s


forwardYield :: Equity -> Equity -> Yield
forwardYield (Equity old) (Equity new) = Yield (new / old)

