{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Report.Pretty where

import Text.Printf (printf)

import qualified Data.List as List

import Data.Time.Clock (UTCTime, NominalDiffTime)

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty = show

instance Pretty Integer where
  pretty = show

instance Pretty Double where
  pretty = show

instance Pretty UTCTime where
  pretty = show

instance Pretty NominalDiffTime where
  pretty dt =
    let y :: Double
        y = realToFrac dt
        sec = 1
        min = 60*sec
        hour = 60*min
        day = 24*hour
        month = 30*day
        year = 365*month
    in case y of
         x | x < min -> printf "%.2fsec" x
         x | x < hour -> printf "%.2fmin" (x / min)
         x | x < day -> printf "%.2fh" (x / hour)
         x | x < month -> printf "%.2fd" (x / day)
         x | x < year -> printf "%.2fmonths" (x / month)
         x -> printf "%.2fyears" (x / year)

instance Pretty String where
  pretty = id

instance (Pretty a) => Pretty (Maybe a) where
  pretty Nothing = "N/A"
  pretty (Just x) = pretty x

instance (Pretty a) => Pretty [a] where
  pretty = List.intercalate ", " . map pretty
