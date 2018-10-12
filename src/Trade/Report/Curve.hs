{-# LANGUAGE TypeFamilies #-}


module Trade.Report.Curve where

import Data.Vector (Vector)

class Curve a where
  type CurveTy a :: *
  curve :: a -> Vector (CurveTy a, Double)
