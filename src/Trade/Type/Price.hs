{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}



module Trade.Type.Price where


import Prelude hiding ((+), (*), (/))
import qualified Prelude as Prelude

import qualified Graphics.Rendering.Chart.Easy as E

newtype Price = Price {
  unPrice :: Double
  } deriving (Show, Eq, Ord, E.PlotValue)
