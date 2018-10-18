{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trade.Type.Equity where

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Report.Pretty

-- | Equity that you own at some point in time.
newtype Equity = Equity {
  unEquity :: Double
  } deriving (Show, Read, Eq, Ord, Num, E.PlotValue)

instance Pretty Equity where
  pretty (Equity x) = "Equity=" ++ show x
