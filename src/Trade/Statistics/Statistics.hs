{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Trade.Statistics.Statistics where


import Trade.Type.Yield (Yield(..))

import Trade.Report.Pretty (Pretty, pretty)


data Statistics t ohlc = Statistics {
  statDuration :: t
  , statYield :: ohlc
  }


formatStat :: (Pretty t, Pretty ohlc) => Statistics t ohlc -> [String]
formatStat (Statistics x y) = [ pretty x, pretty y]

formatYield :: Yield ohlc -> [String]
formatYield (Yield dt y) = [ pretty y, pretty dt ]

