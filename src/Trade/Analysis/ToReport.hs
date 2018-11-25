{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.ToReport where

import Control.Monad

import qualified Trade.Report.Report as Rep

import Data.Monoid ((<>), mempty)

-- We use IO here because charting needs IO.
-- We rather like to serialize charts into memory,
-- but the library only allows for serialization to disk.
class ToReport a where
  toReport :: a -> Rep.HtmlIO

instance ToReport () where
  toReport _ = mempty

instance (ToReport a) => ToReport (Maybe a) where
  toReport x =
    case x of
      Nothing -> mempty
      Just y -> toReport y

instance (ToReport a) => ToReport [a] where
  toReport = mconcat . map toReport

instance (ToReport a, ToReport b) => ToReport (a, b) where
  toReport (x, y) = toReport x <> toReport y


data OptimizationData optInput optOutput = OptimizationData {
  optimizationInput :: optInput
  , optimizationOutput :: optOutput 
  }

data BacktestData backtestInput backtestOutput = BacktestData {
  backtestInput :: backtestInput
  , backtestOutput :: backtestOutput
  }


noBacktestDataReport :: Rep.HtmlIO
noBacktestDataReport = do
  Rep.subheader "Backtest Result"
  Rep.text "No impulse generator found. No backtest done."
  
report ::
  (ToReport (OptimizationData optInp optOut)
  , ToReport (BacktestData backInp backOut)) =>
  String -> OptimizationData optInp optOut -> Maybe (BacktestData backInp backOut) -> Rep.HtmlIO
report ttle opt back =
  let title = Rep.header ttle
      optRep = toReport opt
      backRep = maybe noBacktestDataReport toReport back
  in liftM3 (\a b c -> a <> b <> c) title optRep backRep
