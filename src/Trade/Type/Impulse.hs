{-# LANGUAGE FlexibleInstances #-}


module Trade.Type.Impulse where

import Trade.Report.Pretty

-- | Impulse at some point in time.
data Impulse =
  Buy
  | Sell deriving (Show, Eq)

instance Pretty Impulse where
  pretty = show

instance Pretty (Maybe Impulse) where
  pretty Nothing = "-"
  pretty (Just x) = pretty x
