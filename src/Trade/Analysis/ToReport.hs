

module Trade.Analysis.ToReport where

import qualified Trade.Report.Report as Report


class ToReport a where
  toReport :: a -> [Report.ReportItem]

instance ToReport () where
  toReport _ = []

instance (ToReport a) => ToReport (Maybe a) where
  toReport x =
    case x of
      Nothing -> []
      Just y -> toReport y

instance (ToReport a) => ToReport [a] where
  toReport = concatMap toReport


report :: (ToReport optOut, ToReport backOut) => optOut -> backOut -> [Report.ReportItem]
report opt back =
  let title = Report.header "Report"
      optTitle = Report.subheader "Optimization Results"
      backTitle = Report.subheader "Backtest Results"
      optRep = toReport opt
      backRep = toReport back
  in title : (optTitle : optRep) ++ (backTitle : backRep)


