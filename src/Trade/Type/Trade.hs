

module Trade.Type.Trade where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Trade.Type.Position (Position)

data Trade stgy t ohlc = Trade {
  tradePosition :: Position
  , ticker :: Vector (t, ohlc)
  } deriving (Show)

instance Functor (Trade stgy t) where
  fmap f (Trade ts vs) = Trade ts (Vec.map (fmap f) vs)

newtype TradeList stgy t ohlc = TradeList {
  unTradeList :: [Trade stgy t ohlc]
  } deriving (Show)

instance Functor (TradeList stgy t) where
  fmap f (TradeList tl) = TradeList (map (fmap f) tl)

