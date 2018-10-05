{-# LANGUAGE FlexibleContexts #-}


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

instance (ToReport a, ToReport b) => ToReport (a, b) where
  toReport (x, y) = toReport x ++ toReport y

newtype ReportString = ReportString {
  reportString :: String
  } deriving (Show)

instance ToReport ReportString where
  toReport (ReportString str) = [Report.text str]


data OptimizationData optInput optOutput = OptimizationData {
  optimizationInput :: optInput
  , optimizationOutput :: optOutput
  }

data BacktestData backtestInput backtestOutput = BacktestData {
  backtestInput :: backtestInput
  , backtestOutput :: backtestOutput
  }
  
report ::
  (ToReport (OptimizationData optInp optOut), ToReport (BacktestData backInp backOut)) =>
  String -> OptimizationData optInp optOut -> BacktestData backInp backOut -> [Report.ReportItem]
report ttle opt back =
  let title = Report.header ttle
      optRep = toReport opt
      backRep = toReport back
  in title : optRep ++ backRep


