{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Report.Line where


import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Signal(..))


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


lineHelper :: (a -> b) -> String -> Signal t a -> L [(t, b)]
lineHelper un str (Signal ps) = L str (Vec.toList (Vec.map (fmap un) ps))


instance Line (Signal t Equity) where
  type TyX (Signal t Equity) = t
  type TyY (Signal t Equity) = Double
  line = lineHelper unEquity

instance Line (Signal t Price) where
  type TyX (Signal t Price) = t
  type TyY (Signal t Price) = Double
  line = lineHelper unPrice
