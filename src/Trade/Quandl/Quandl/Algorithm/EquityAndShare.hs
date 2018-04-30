{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trade.Quandl.Quandl.Algorithm.EquityAndShare where


import qualified Data.ByteString.Char8 as BS

import Data.Csv (FromField, parseField)


newtype Equity = Equity { unEquity :: Double } deriving (Show, Eq, Ord, Num)

newtype PricePerShare = PricePerShare { unPricePerShare :: Double } deriving (Show, Eq, Ord, Num)

newtype Share = Share { unShare :: Integer } deriving (Show, Eq, Ord, Num)

newtype Yield = Yield { unYield :: Double } deriving (Show, Eq, Ord, Num)

class EPS a where
  type Ty a :: *
    
  (.*) :: a -> Ty a -> Equity


instance EPS Share where
  type Ty Share = PricePerShare

  (Share s) .* (PricePerShare pps) = Equity (fromIntegral s * pps)

instance EPS PricePerShare where
  type Ty PricePerShare = Share 

  (PricePerShare pps) .* (Share s) = Equity (fromIntegral s * pps)


sharesPerEquity :: Equity -> PricePerShare -> Share
sharesPerEquity (Equity e) (PricePerShare pps) = Share (floor (e/pps))

(./) :: Equity -> PricePerShare -> Share
(./) = sharesPerEquity

totalEquity :: Equity -> PricePerShare -> Share -> Equity
totalEquity e pps s = e + pps .* s

instance FromField PricePerShare where
  parseField =  return . PricePerShare . read . BS.unpack


forwardYield :: Equity -> Equity -> Yield
forwardYield (Equity old) (Equity new) = Yield (new / old)

