

module Trade.Type.Trade where

import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Time.Clock (UTCTime)

import Trade.Type.State (State)

data Trade ohlc = Trade {
  tradeState :: State
  , ticker :: Vector (UTCTime, ohlc)
  } deriving (Show)

instance Functor Trade where
  fmap f (Trade ts vs) = Trade ts (Vec.map (fmap f) vs)

newtype TradeList ohlc = TradeList {
  unTradeList :: [Trade ohlc]
  } deriving (Show)

instance Functor TradeList where
  fmap f (TradeList tl) = TradeList (map (fmap f) tl)

