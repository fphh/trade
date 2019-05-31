{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}


module Trade.Report.Line where

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Type.Equity (Equity(..))
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Signal(..))


type family XTy line :: *


class Line a where
  line :: String -> a -> E.EC l (E.PlotLines (XTy a) Double)



type instance XTy (Vector (x, y)) = x
  
instance Line (Vector (x, Double)) where
  line str vs = E.line str [Vec.toList vs]


instance Line (Vector (x, Price)) where
  line str vs = E.line str [Vec.toList (Vec.map (fmap unPrice) vs)]




type instance XTy [(x, y)] = x

instance Line [(x, Double)] where
  line str vs = E.line str [vs]



type instance XTy (Signal t y) = t

instance Line (Signal t Double) where
  line str (Signal ps) = E.line str [Vec.toList ps]

instance Line (Signal t Equity) where
  line str (Signal ps) = E.line str [Vec.toList (Vec.map (fmap unEquity) ps)]

instance Line (Signal t Price) where
  line str (Signal ps) = E.line str [Vec.toList (Vec.map (fmap unPrice) ps)]
