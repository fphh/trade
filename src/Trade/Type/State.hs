

module Trade.Type.State where

import Trade.Report.Pretty


-- | The trader's state.
-- TODO: Being short.
data State =
  Long
  -- -- | Short
  | NoPosition
  deriving (Show, Eq, Ord)

instance Pretty State where
  pretty = show
