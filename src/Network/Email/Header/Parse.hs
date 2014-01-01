{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Header parsers.
module Network.Email.Header.Parse
    (  -- * Whitespace
      cfws
       -- * Date and time
    , dateTime
      -- * Addresses
    , address
    , mailbox
    , mailboxList
    , recipient
    , recipientList
      -- * Message IDs
    , messageID
    , messageIDList
      -- * Text
    , phrase
    , phraseList
    , unstructured
      -- * MIME
    , mimeVersion
    , contentType
    ) where

import           Control.Applicative
import           Data.Attoparsec             (Parser)
import qualified Data.Attoparsec             as A
import qualified Data.Attoparsec.Char8       as A8
import           Data.Attoparsec.Combinator
import           Data.List
import qualified Data.Map.Strict             as Map
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import           Data.Text.Encoding

import Network.Email.Header.Parse.Internal
import Network.Email.Types                 hiding (mimeType)

-- | Parse a date and time. Currently, non-numeric timezones (such as \"PDT\")
-- are considered equivalent to UTC time.
dateTime :: Parser ZonedTime
dateTime = do
    wday  <- optional dayOfWeek
    zoned <- zonedTime
    let (_, _, expected) =
            toWeekDate . localDay . zonedTimeToLocalTime $ zoned
    case wday of
        Just actual | actual /= expected
          -> fail "day of week does not match date"
        _ -> return zoned
  where
    dayOfWeek = dayName <* symbol ','
    localTime = LocalTime <$> date <*> timeOfDay
    zonedTime = ZonedTime <$> localTime <*> timeZone

    date      = do
        d <- lexeme A8.decimal
        m <- month
        y <- year
        parseMaybe "invalid date" $ fromGregorianValid y m d

    year      =              number 4
            <|> (+ 1900) <$> number 3
            <|> adjust   <$> number 2
      where
        adjust n | n < 50    = 2000 + n
                 | otherwise = 1900 + n

    timeOfDay = do
        h <- number 2
        m <- symbol ':' *> number 2
        s <- option (0 :: Int) (symbol ':' *> number 2)
        parseMaybe "invalid time of day" $
            makeTimeOfDayValid h m (fromIntegral s)

    timeZone  = minutesToTimeZone <$> timeZoneOffset
            <|> return utc
      where
        timeZoneOffset = lexeme . A8.signed $ do
            hh <- digits 2
            mm <- digits 2
            if mm >= 60
                then fail "invalid time zone"
                else return $ hh * 60 + mm

    listIndex = lexeme . choice . map (\(n, s) -> n <$ A.string s) . zip [1..]
    dayName   = listIndex [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]
    month     = listIndex [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
                          , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
                          ]

-- | Parse an email address in angle brackets.
angleAddr :: Parser Address
angleAddr = Address <$> angleAddrSpec

-- | Parse an email address.
address :: Parser Address
address = Address <$> addrSpec

-- | Parse a 'Mailbox'.
mailbox :: Parser Mailbox
mailbox = Mailbox <$> optional phrase <*> angleAddr
      <|> Mailbox Nothing <$> address

-- | Parse a list of @'Mailbox'es@.
mailboxList :: Parser [Mailbox]
mailboxList = commaSep mailbox

-- | Parse a 'Recipient'.
recipient :: Parser Recipient
recipient = Group <$> phrase <* symbol ':' <*> mailboxList <* symbol ';'
        <|> Individual <$> mailbox

-- | Parse a list of @'Recipient's@.
recipientList :: Parser [Recipient]
recipientList = commaSep recipient

-- | Parse a message identifier.
messageID :: Parser MessageID
messageID = MessageID <$> angleAddrSpec

-- | Parse a list of message identifiers.
messageIDList :: Parser [MessageID]
messageIDList = many1 messageID

-- | Combine a list of text elements (atoms, quoted strings, encoded words,
-- etc.) into a larger phrase.
fromElements :: [T.Text] -> L.Text
fromElements = L.fromChunks . intersperse (T.singleton ' ')

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
-- White space is reduced to a single space, except when part of an encoded
-- word.
unstructured :: Parser L.Text
unstructured = fromElements <$> many element
  where
    element = T.concat     <$> many1 encodedWord
          <|> decodeLatin1 <$> textToken

-- | Parse the MIME version (which should be 1.0).
mimeVersion :: Parser (Int, Int)
mimeVersion = (,) <$> number 1 <* symbol '.' <*> number 1

-- | Parse the content type.
contentType :: Parser (MimeType, Parameters)
contentType = (,) <$> mimeType <*> parameters
  where
    mimeType   = MimeType <$> token <* symbol '/' <*> token
    parameters = Map.fromList <$> many (symbol ';' *> parameter)
    parameter  = (,) <$> token <* symbol '=' <*> (token <|> quotedString)
