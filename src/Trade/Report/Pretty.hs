{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Report.Pretty where


import qualified Data.List as List

import Data.Time.Clock (UTCTime, NominalDiffTime)

import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)

import Formatting (Format, Buildable, (%), format, build, mapf, fixed, int)
import Formatting.Time (datetime, diff, days, minutes, hours, years, decimals, seconds)

import Data.String (fromString)


import Debug.Trace

formatf :: (Real b) => (a -> b) -> String -> a -> String
formatf f str = Text.unpack . format (mapf f (fixed 6 % fromString str))


class Pretty a where
  pretty :: a -> String


instance Pretty UTCTime where
  pretty = Text.unpack . format datetime
  
instance Pretty NominalDiffTime where
  pretty dt =
    let y :: Double
        y = realToFrac dt
        sec = 1
        min = 60*sec
        hour = 60*min
        day = 24*hour
        month = 30*day

        fmt =
          case y of
            x | x < min -> seconds 2 % fromString " sec"
            x | x < hour -> minutes 2 % fromString " min"
            x | x < day -> hours 2 % fromString " h"
            x | x < month -> days 2 % fromString " d"
            x -> years 2 % fromString " years"
    
    in Text.unpack (format fmt dt)




instance Pretty Int where
  pretty = Text.unpack . format int
  
instance Pretty Double where
  pretty = Text.unpack . format (fixed 6)  

instance Pretty Bool where
  pretty = show
 

instance Pretty a => Pretty (Maybe a) where
  pretty (Just x) = pretty x
  pretty Nothing = "n/a"
