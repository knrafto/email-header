-- | Rendering of date and time values.
module Network.Email.Render.Header.DateTime
    ( dateTime
    ) where

import Data.ByteString.Lazy.Builder
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale

-- | Render a date and time.
dateTime :: ZonedTime -> Builder
dateTime = string7 . formatTime defaultTimeLocale "%a, %e %b %0Y %z"
