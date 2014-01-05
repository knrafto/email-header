{-# LANGUAGE OverloadedStrings #-}
module Network.Email.Header.Builder.Format
    ( -- * Date and time
      dateTime
    ) where

import Data.String
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

import Network.Email.Format

-- | Render a date and time. TODO: proper spacing
dateTime :: ZonedTime -> Doc
dateTime = fromString . formatTime defaultTimeLocale rfc822DateFormat
