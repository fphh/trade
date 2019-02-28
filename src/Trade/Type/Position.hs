
module Trade.Type.Position where

import qualified Test.QuickCheck as QC

import Trade.Report.Pretty


-- | The trader's position.
-- If this means short or long is defined by the strategy.
data Position =
  Invested
  | NotInvested
  deriving (Show, Eq, Ord)

instance QC.Arbitrary Position where
  arbitrary = QC.elements [Invested, NotInvested]

instance Pretty Position where
  pretty = show
