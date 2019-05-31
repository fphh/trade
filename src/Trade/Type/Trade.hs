

module Trade.Type.Trade where

import Data.Time.Clock (UTCTime)

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Trade.Type.Position (Position)

data Trade stgy ohlc = Trade {
  tradePosition :: Position
  , ticker :: Vector (UTCTime, ohlc)
  } deriving (Show)

instance Functor (Trade stgy) where
  fmap f (Trade ts vs) = Trade ts (Vec.map (fmap f) vs)

newtype TradeList stgy ohlc = TradeList {
  unTradeList :: [Trade stgy ohlc]
  } deriving (Show)

instance Functor (TradeList stgy) where
  fmap f (TradeList tl) = TradeList (map (fmap f) tl)


emptyTradeList :: TradeList stgy ohlc
emptyTradeList = TradeList []
