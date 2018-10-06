{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Trade.Report.Style where


import qualified Graphics.Rendering.Chart.Easy as E

data AxisConfig a = AxisConfig {
  axisLayout :: E.LayoutAxis a
  , axisVisibility :: E.AxisVisibility
--  , axisStyle :: E.AxisStyle
  , axisFn :: Maybe (E.AxisFn a)
  }

axisConfDef ::(E.PlotValue y, RealFloat y, Show y, Num y, E.Default (E.AxisData y)) =>  AxisConfig y
axisConfDef = AxisConfig E.def E.def {- E.def -} Nothing


axTitle :: (E.PlotValue a) => String -> AxisConfig a
axTitle str =
  let al = E.laxis_title E..~ str $ E.def
  in AxisConfig al E.def Nothing

  
impulseAxisConf :: AxisConfig Double
impulseAxisConf =
  let al = E.laxis_style E..~ (E.axis_grid_style E..~ (E.line_width E..~ 0 $ E.def) $ E.axis_line_style E..~ (E.line_width E..~ 0 $ E.def) $ E.def)
           $ E.def
      av = E.axis_show_labels E..~ False
           $ E.axis_show_ticks E..~ False
           $ E.def
      af = E.scaledAxis E.def (-1,10)
  in AxisConfig al av (Just af)


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
  in AxisConfig al av {- E.def -} (Just af)
                  
