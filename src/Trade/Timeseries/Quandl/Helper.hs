{-# LANGUAGE TupleSections #-}

module Trade.Timeseries.Quandl.Helper where
  
import Data.Csv (FromField, NamedRecord, Parser, (.:))

import qualified Data.ByteString.Char8 as BS


(.::) :: (FromField a) => NamedRecord -> String -> Parser a
x .:: y = x .: (BS.pack y)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a:as) = map (a,) as ++ pairs as

