

module Trade.Type.Step.Commission where

import Trade.Report.Pretty

-- | Apply fees to the trade volume.
newtype Commission = Commission {
  unCommission :: Double -> Double
  }

instance Pretty Commission where
  pretty = ("comm. at $1=" ++) . show . ($ 1) . unCommission


noCommission :: Commission
noCommission = Commission (const 0)
