{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Trade.Report.Style where

import qualified Graphics.Rendering.Chart.Easy as E

data AxisConfig a b = AxisConfig {
  xAxisLayout :: E.LayoutAxis a
  , yAxisLayout :: E.LayoutAxis b
  , axisVisibility :: E.AxisVisibility
--  , axisStyle :: E.AxisStyle
  , axisFn :: Maybe (E.AxisFn a)
  }

-- axisConfDef ::(E.PlotValue y, RealFloat y, Show y, Num y, E.Default (E.AxisData y)) =>  AxisConfig y
-- axisConfDef = AxisConfig E.def E.def E.def {- E.def -} Nothing


axTitle :: (E.PlotValue x, E.PlotValue y) => String -> String -> AxisConfig x y
axTitle strX strY =
  let al = E.laxis_title E..~ strX $ E.def
      bl = E.laxis_title E..~ strY $ E.def
  in AxisConfig al bl E.def Nothing

  
impulseAxisConf :: (E.PlotValue a, E.PlotValue b) => AxisConfig a b
impulseAxisConf =
  let xal = E.laxis_style E..~ (E.axis_grid_style E..~ (E.line_width E..~ 0 $ E.def) $ E.axis_line_style E..~ (E.line_width E..~ 0 $ E.def) $ E.def)
           $ E.def
      -- yal = E.axis_show_labels E..~ True
      --      $ E.def
      av = id -- E.axis_show_labels E..~ True
           --  $ E.axis_show_ticks E..~ True
           --  $ E.axis_labels E..~ [[(-1, "Buy"), (1, "Sell")]]
           $ E.def
      af = Nothing -- E.scaledAxis E.def (-1,10)
  in AxisConfig xal E.def av af -- (Just af)

{-
instance E.Default (E.AxisData Double) where
  def = E.AxisData E.def E.def E.def E.def E.def E.def


instance (E.PlotValue a, RealFloat a, Show a, E.Default (E.AxisData a)) => E.Default (AxisConfig a) where
  def = axisConfDef

exampleAxisConf ::(E.PlotValue y, RealFloat y, Show y, Num y) =>  AxisConfig y
exampleAxisConf =
  let al = E.laxis_title E..~ "Impulse Signal"
           $ E.def
      av = E.axis_show_labels E..~ False
           $ E.axis_show_ticks E..~ False
           $ E.def
      af = E.scaledAxis E.def (-1,10)
  in AxisConfig al E.def av {- E.def -} (Just af)
                  -}

colors :: [E.AlphaColour Double]
colors = map E.opaque [ E.red, E.blue, E.green, E.magenta, E.orange, E.darkcyan, E.black, E.gray, E.purple, E.pink ] ++ colors
