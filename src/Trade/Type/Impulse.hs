{-# LANGUAGE FlexibleInstances #-}


module Trade.Type.Impulse where


import qualified Graphics.Rendering.Chart.Easy as E

import Trade.Report.Pretty

-- | Impulse at some point in time.
data Impulse =
  Buy
  | Sell deriving (Show, Eq, Ord)

instance Pretty Impulse where
  pretty = show

instance Pretty (Maybe Impulse) where
  pretty Nothing = "-"
  pretty (Just x) = pretty x


instance E.PlotValue (Maybe Impulse) where
  toValue x =
    case x of
      Nothing -> 0
      Just Sell -> 1
      Just Buy -> -1

  fromValue = error "fromValue: E.PlotValue (Maybe Impulse)"

  autoAxis _ =
    let f _ = ["Buy", "", "Sell"]
        ax = E.makeAxis f ([Just Buy, Nothing, Just Sell], [Just Buy, Nothing, Just Sell], [Just Buy, Nothing, Just Sell])
        g = E._axis_viewport ax
    in (E.axisGridHide ax) { E._axis_viewport = \x y -> g x y / 2 }


invert :: Impulse -> Impulse
invert Sell = Buy
invert Buy = Sell
