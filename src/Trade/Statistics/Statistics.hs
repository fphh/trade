{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Trade.Statistics.Statistics where


import Data.Time.Clock (NominalDiffTime)

import Text.Printf (printf, PrintfArg)

import Trade.Type.Yield (Yield(..))

import Trade.Report.Pretty (pretty)


data Statistics t ohlc = Statistics {
  statDuration :: t
  , statYield :: ohlc
  }


class FormatStat t where
  formatStat :: (PrintfArg y) => Statistics t y -> [String]

instance FormatStat Double where
  formatStat (Statistics x y) = [ printf "%.8f" y, printf "%.8f" x ]
  formatStat (Statistics x y) = [ printf "%.8f" y, printf "%.8f" x ]

instance FormatStat NominalDiffTime where
  formatStat (Statistics dt y) = [ printf "%.8f" y, pretty dt ]

formatYield :: Yield ohlc -> [String]
formatYield (Yield dt y) = [ printf "%.8f" y, pretty dt ]

