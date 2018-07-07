

module Trade.Type.Commission where

import Trade.Report.Pretty

newtype Commission = Commission {
  unCommission :: Double -> Double
  }

instance Pretty Commission where
  pretty = ("comm. at $1=" ++) . show . ($ 1) . unCommission

