{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Trade.TStatistics.Statistics where


import Data.Time.Clock (UTCTime, NominalDiffTime)

import Text.Printf (printf, PrintfArg)

import Trade.Type.Bars (BarNo)
import Trade.Type.Yield (Yield(..))

import Trade.Report.Pretty (Pretty, pretty)


newtype DeltaTyStats t = DeltaTyStats {
  unDeltaTyStats :: Double
  }

instance Pretty (DeltaTyStats BarNo) where
  pretty = printf "b%.8f" . unDeltaTyStats

instance Pretty (DeltaTyStats UTCTime) where
  pretty (DeltaTyStats t) = pretty (realToFrac t :: NominalDiffTime)


data Statistics t y = Statistics {
  statDuration :: t
  , statYield :: y
  }

class FormatStat t where
  formatStat :: (PrintfArg y) => Statistics t y -> [String]

instance FormatStat Double where
  formatStat (Statistics x y) = [ printf "%.8f" y, printf "%.8f" x ]

instance (Pretty (DeltaTyStats t)) => FormatStat (DeltaTyStats t) where
  formatStat (Statistics dt y) = [ printf "%.8f" y, pretty dt ]


formatYield :: (Pretty t) =>  Yield t ohlc -> [String]
formatYield (Yield dt y) = [ printf "%.8f" y, pretty dt ]

