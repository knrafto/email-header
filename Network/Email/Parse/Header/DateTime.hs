{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of date and time values.
module Network.Email.Parse.Header.DateTime
    ( dateTime
    ) where

import           Control.Applicative
import           Data.Attoparsec              (Parser)
import qualified Data.Attoparsec              as A
import qualified Data.Attoparsec.Char8        as A8
import           Data.Attoparsec.Combinator
import           Data.Time
import           Data.Time.Calendar.WeekDate

import           Network.Email.Parse.Header.Internal

-- | Parse a date and time. Currently, non-numeric timezones (such as \"PDT\")
-- are considered equivalent to UTC time.
dateTime :: Parser ZonedTime
dateTime = do
    wday  <- optional dayOfWeek
    zoned <- zonedTime

    let (_, _, expected) =
            toWeekDate . localDay . zonedTimeToLocalTime $ zoned
    case wday of
        Just actual | actual /= expected -> fail
            "day of week does not match date"
        _                                -> return ()

    return zoned
  where
    comma     = 44
    colon     = 58

    dayOfWeek = lexeme dayName <* character comma
    localTime = LocalTime <$> date <*> timeOfDay
    zonedTime = ZonedTime <$> localTime <*> timeZone

    date      = do
        d <- lexeme A8.decimal
        m <- lexeme month
        y <- lexeme year
        failNothing "invalid date" $ fromGregorianValid y m d

    year      =              digits 4
            <|> (+ 1900) <$> digits 3
            <|> adjust   <$> digits 2
      where
        adjust n | n < 50    = 2000 + n
                 | otherwise = 1900 + n

    timeOfDay = do
        h <- lexeme (digits 2)
        m <- character colon *> lexeme (digits 2)
        s <- option (0 :: Int) (character colon *> lexeme (digits 2))
        failNothing "invalid time of day" $
            makeTimeOfDayValid h m (fromIntegral s)

    timeZone  = minutesToTimeZone <$> timeZoneOffset <|> return utc
      where
        timeZoneOffset = A8.signed $ do
            hh <- digits 2
            mm <- digits 2
            if mm >= 60
                then fail "invalid time zone"
                else return $ hh * 60 + mm

    listIndex = choice . map (\(n, s) -> n <$ A.string s) . zip [1..]
    dayName   = listIndex [ "Mon", "Tue", "Wed", "Thu"
                          , "Fri", "Sat", "Sun"
                          ]
    month     = listIndex [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
                          , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
                          ]
