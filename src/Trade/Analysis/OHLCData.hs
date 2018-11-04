{-# LANGUAGE TypeFamilies #-}

module Trade.Analysis.OHLCData where

class OHLCData d where
  type OHLCDataTy d :: *


-- | Like '()', but maybe better.
data NoOHLC = NoOHLC
