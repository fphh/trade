
module Trade.Timeseries.Time where

import Control.Monad.Fail

import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)

import Data.Csv (FromField, parseField)

import qualified Data.ByteString.Char8 as BS

parseDate :: (Monad m, MonadFail m) => String -> m UTCTime
parseDate = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing)

{-
parseDate :: String -> Maybe UTCTime
parseDate str =
  let r = parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%QZ")) str
      s = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) str
  in maybe s Just r
-}

instance FromField UTCTime where
  parseField = parseDate . BS.unpack
