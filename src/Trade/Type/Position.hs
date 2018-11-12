

module Trade.Type.Position where

import Trade.Report.Pretty


-- | The trader's position.
-- TODO: Being short.
data Position =
  Long
  -- -- | Short
  | NoPosition
  deriving (Show, Eq, Ord)

instance Pretty Position where
  pretty = show