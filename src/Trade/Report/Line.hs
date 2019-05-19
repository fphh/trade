{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Trade.Report.Line where

import Data.Time.Clock (UTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Type.Bars (BarNo)
import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Signal(..))


data Line a = Line {
  label :: String
  , timeseries :: a
  } deriving (Show)

{-
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
-}

class ToLine a where
  type XTy a :: *
  type YTy a :: *
  toLine :: Line a -> E.EC l (E.PlotLines (XTy a) (YTy a)) 

instance ToLine [(t, price)] where
  type XTy [(t, price)] = t
  type YTy [(t, price)] = price
  toLine (Line str vs) = E.line str [vs]

instance ToLine (Vector (t, Price)) where
  type XTy (Vector (t, Price)) = t
  type YTy (Vector (t, Price)) = Price
  toLine (Line str vs) = E.line str [Vec.toList vs]

{-
instance ToLine [(BarNo, Double)] where
  type XTy [(BarNo, Double)] = BarNo
  type YTy [(BarNo, Double)] = Double
  toLine (Line str vs) = E.line str [vs]
-}

instance ToLine (Vector (t, price)) => ToLine (Signal t price) where
  type XTy (Signal t price) = XTy (Vector (t, price))
  type YTy (Signal t price) = YTy (Vector (t, price))
  toLine (Line str (Signal vs)) = toLine (Line str vs)
