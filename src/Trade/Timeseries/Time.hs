
module Trade.Timeseries.Time where

import Control.Monad.Fail

import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)

import Data.Csv (FromField, parseField)

import qualified Data.ByteString.Char8 as BS

parseDate :: (Monad m, MonadFail m) => String -> m UTCTime
parseDate = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing)


parseDateISO :: (Monad m, MonadFail m) => String -> m UTCTime
parseDateISO = parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%QZ"))



instance FromField UTCTime where
  parseField = parseDate . BS.unpack
