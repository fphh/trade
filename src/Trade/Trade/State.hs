

module Trade.Trade.State where

import Trade.Report.Pretty


data State =
  Long
  -- | Short
  | NoPosition
  | NoState
  deriving (Show, Eq, Ord)

instance Pretty State where
  pretty = show
