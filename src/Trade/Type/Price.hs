

module Trade.Type.Price where

import Trade.Report.Pretty (Pretty, pretty)


newtype Price = Price {
  unOpen :: Double
  } deriving (Show, Eq, Ord)


instance Pretty Price where
  pretty = show
