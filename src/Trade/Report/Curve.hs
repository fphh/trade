{-# LANGUAGE TypeFamilies #-}


module Trade.Report.Curve where

import Data.Vector (Vector)

class Curve a where
  type Ty a :: *
  curve :: a -> Vector (Ty a, Double)
