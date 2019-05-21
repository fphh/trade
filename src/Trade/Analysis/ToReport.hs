{-# LANGUAGE FlexibleContexts #-}


module Trade.Analysis.ToReport where

import Control.Monad

import Control.Monad.Reader (ReaderT(..))

import Text.Blaze.Html5 (Html)

import qualified Trade.Report.Report as Rep
import Trade.Report.Config (Config, HtmlReader)

import Data.Monoid ((<>), mempty)


class ToReport a where
  toReport :: a -> HtmlReader ()


instance ToReport () where
  toReport _ = ReaderT (const mempty)

{-
instance (ToReport a) => ToReport (Maybe a) where
  toReport = maybe (toReport ()) toReport
-}

instance (ToReport a) => ToReport [a] where
  toReport = fmap mconcat . mapM toReport


data OptimizationData optInput optOutput = OptimizationData {
  optimizationInput :: optInput
  , optimizationOutput :: optOutput 
  }

data BacktestData backtestInput backtestOutput = BacktestData {
  backtestInput :: backtestInput
  , backtestOutput :: backtestOutput
  }


noBacktestDataReport ::HtmlReader ()
noBacktestDataReport = do
  Rep.subheader "Backtest Result"
  Rep.text "No optimized impulse generator found. No backtest done."
  
report ::
  (ToReport (OptimizationData optInp optOut)
  , ToReport (BacktestData backInp backOut)) =>
  String -> OptimizationData optInp optOut -> Maybe (BacktestData backInp backOut) -> HtmlReader ()
report ttle opt back =
  let title = Rep.header ttle
      optRep = toReport opt
      backRep = maybe noBacktestDataReport toReport back
  in liftM3 (\a b c -> a <> b <> c) title optRep backRep
