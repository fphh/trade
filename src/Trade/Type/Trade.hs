

module Trade.Type.Trade where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Trade.Type.Position (Position)

data Trade t ohlc = Trade {
  tradePosition :: Position
  , ticker :: Vector (t, ohlc)
  } deriving (Show)

instance Functor (Trade t) where
  fmap f (Trade ts vs) = Trade ts (Vec.map (fmap f) vs)

newtype TradeList t ohlc = TradeList {
  unTradeList :: [Trade t ohlc]
  } deriving (Show)

instance Functor (TradeList t) where
  fmap f (TradeList tl) = TradeList (map (fmap f) tl)

