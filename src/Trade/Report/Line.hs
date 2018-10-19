{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Report.Line where


import qualified Data.Vector as Vec
import Data.Vector (Vector)

data L a = L String a

class Line a where
  type TyX a :: *
  type TyY a :: *
  line :: String -> a -> L [(TyX a, TyY a)]

instance Line (Vector (x, y)) where
  type TyX (Vector (x, y)) = x
  type TyY (Vector (x, y)) = y
  line str vs = L str (Vec.toList vs)

instance Line [(x, y)] where
  type TyX [(x, y)] = x
  type TyY [(x, y)] = y
  line str vs = L str vs