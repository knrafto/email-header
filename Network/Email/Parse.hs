{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of common header values.
module Network.Email.Parse
    ( phrase
    , phraseList
    , unstructured
    , dateTime
    ) where

import           Control.Applicative
import           Data.Attoparsec              (Parser)
import qualified Data.Attoparsec              as A
import qualified Data.Attoparsec.Char8        as A8
import           Data.Attoparsec.Combinator
import           Data.Char
import           Data.List
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy               as L
import           Data.Text.Lazy.Builder
import           Data.Time
import           Data.Time.Calendar.WeekDate

import           Network.Email.Parse.Internal

-- | Combine a list of text elements (atoms, quoted strings, encoded words,
-- etc.) into a larger phrase.
fromElements :: [T.Text] -> L.Text
fromElements =
    toLazyText . mconcat . intersperse (singleton ' ') .
    map fromText . concatMap splitElement
  where
    splitElement = filter (not . T.null) . T.split isSep
    isSep c      = isControl c || isSpace c

-- | Parse a phrase. Adjacent encoded words are concatenated. White space
-- is reduced to a single space, except when quoted or part of an encoded
-- word.
phrase :: Parser L.Text
phrase = fromElements <$> many1 element
  where
    element = T.concat     <$> many1 encodedWord
          <|> decodeLatin1 <$> quotedString
          <|> decodeLatin1 <$> dotAtom

-- | Parse a comma-separated list of phrases.
phraseList :: Parser [L.Text]
phraseList = commaSep phrase

-- | Parse unstructured text. Adjacent encoded words are concatenated.
-- White space is reduced to a single space.
unstructured :: Parser L.Text
unstructured = fromElements <$> many element
  where
    element = T.concat     <$> many1 encodedWord
          <|> decodeLatin1 <$> textToken

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
    dayOfWeek = lexeme dayName <* symbol ","
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
        m <- symbol ":" *> lexeme (digits 2)
        s <- option (0 :: Int) (symbol ":" *> lexeme (digits 2))
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
