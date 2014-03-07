{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Header parsers.
module Network.Email.Header.Parse.Internal
    ( -- * Whitespace
      fws
    , cfws
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
    , contentTransferEncoding
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec              (Parser)
import qualified Data.Attoparsec              as A
import qualified Data.Attoparsec.Char8        as A8
import           Data.Attoparsec.Combinator
import           Data.Bits
import qualified Data.ByteString              as B
import qualified Data.ByteString.Base64       as Base64
import           Data.ByteString.Internal     (w2c)
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as L (toStrict)
import           Data.List
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Data.Time.Calendar.WeekDate
import qualified Data.Text                    as T
import           Data.Text.Encoding
import           Data.Text.ICU.Convert
import qualified Data.Text.Lazy               as L (Text, fromChunks)
import           Data.Word

import           Network.Email.Charset
import           Network.Email.Header.Types   hiding (mimeType)

infixl 3 <+>

-- | Concatenate the results of two parsers.
(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

-- | Repeat and concatenate.
concatMany :: (Alternative f, Monoid a) => f a -> f a
concatMany p = mconcat <$> many p

-- | Return a 'Just' value, and 'fail' a 'Nothing' value.
parseMaybe :: Monad m => String -> Maybe a -> m a
parseMaybe s = maybe (fail s) return

-- | Return a 'Right' value, and 'fail' a 'Left' value.
parseEither :: Monad m => Either String a -> m a
parseEither = either fail return

-- | Run a 'Builder' as a strict 'B.ByteString'.
toByteString :: Builder -> B.ByteString
toByteString = L.toStrict . toLazyByteString

-- | Skip folding whitespace.
fws :: Parser ()
fws = A8.skipSpace

-- | Parse a comment, including all nested comments.
comment :: Parser B.ByteString
comment = A8.char '(' *> A.scan (0 :: Int, False) f <* A8.char ')'
  where
    f (!n, True ) _ = Just (n, False)
    f (!n, False) w = case w2c w of
        '('  -> Just (n + 1, False)
        ')'  -> if n == 0 then Nothing else Just (n - 1, False)
        '\\' -> Just (n, True)
        _    -> Just (n, False)

-- | Skip any comments or folding whitespace.
cfws :: Parser ()
cfws = () <$ fws `sepBy` comment

-- | Parse a value followed by whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* cfws

-- | Parse a character followed by whitespace.
symbol :: Char -> Parser Char
symbol = lexeme . A8.char

-- | Quickly (and unsafely) convert a digit to the number it represents.
fromDigit :: Integral a => Word8 -> a
fromDigit w = fromIntegral (w - 48)

-- | Parse a fixed number of digits.
digits :: Integral a => Int -> Parser a
digits 0 = return 0
digits 1 = fromDigit <$> A.satisfy A8.isDigit_w8
digits n = do
    s <- A.take n
    unless (B.all A8.isDigit_w8 s) $
        fail $ "expected " ++ show n ++ " digits"
    return $ B.foldl' (\a w -> 10*a + fromDigit w) 0 s

-- | Parse a number lexeme with a fixed number of digits.
number :: Integral a => Int -> Parser a
number = lexeme . digits

-- | Parse a hexadecimal pair.
hexPair :: Parser Word8
hexPair = decode <$> hexDigit <*> hexDigit
  where
    decode a b      = shiftL a 4 .|. b
    hexDigit        = fromHexDigit <$> A.satisfy isHexDigit
    isHexDigit w    = w >= 48 && w <= 57
                   || w >= 64 && w <= 70
                   || w >= 97 && w <= 102
    fromHexDigit w
        | w >= 97   = w - 87
        | w >= 64   = w - 55
        | otherwise = w - 48

-- | Parse an token lexeme consisting of all printable characters, but
--  disallowing the specified special characters.
tokenWith :: String -> Parser B.ByteString
tokenWith specials = lexeme (A.takeWhile1 isAtom)
  where
    isAtom w = w <= 126 && w >= 33 && A.notInClass specials w

-- | Parse an atom, which contains ASCII letters, digits, and the
-- characters @"!#$%&\'*+-/=?^_`{|}~"@.
atom :: Parser B.ByteString
atom = tokenWith "()<>[]:;@\\\",."

-- | Parse a dot-atom, or an atom which may contain periods.
dotAtom :: Parser B.ByteString
dotAtom = tokenWith "()<>[]:;@\\\","

-- | A MIME token, which contains ASCII letters, digits, and the
-- characters @"!#$%\'*+-^_`{|}~."@.
token :: Parser B.ByteString
token = tokenWith "()<>@,;:\\\"/[]?="

-- | Parse a quoted-string.
quotedString :: Parser B.ByteString
quotedString = lexeme $
    toByteString <$ A8.char '"' <*> concatMany quotedChar <* A8.char '"'
  where
    quotedChar = mempty <$ A.string "\r\n" 
             <|> word8 <$ A8.char '\\' <*> A.anyWord8
             <|> char8 <$> A8.satisfy (/= '"')

-- | Parse an encoded word, as per RFC 2047.
encodedWord :: Parser T.Text
encodedWord = do
    _       <- A.string "=?"
    charset <- B8.unpack <$> tokenWith "()<>@,;:\"/[]?.="
    _       <- A8.char '?'
    method  <- decodeMethod
    _       <- A8.char '?'
    enc     <- method
    _       <- A.string "?="

    converter <- parseMaybe "charset not found" $ lookupConverter charset
    return $ toUnicode converter enc
  where
    decodeMethod = quoted       <$ A.satisfy (`B.elem` "Qq")
               <|> base64String <$ A.satisfy (`B.elem` "Bb")

    quoted       = toByteString <$> concatMany quotedChar

    quotedChar   = char8 ' ' <$ A8.char '_'
               <|> word8 <$ A8.char '=' <*> hexPair
               <|> char8 <$> A8.satisfy (not . isBreak)

    isBreak c    = A8.isSpace c || c == '?'

    base64String = do
        s <- A8.takeWhile (/= '?')
        parseEither (Base64.decode s)

-- | Return a quoted string as-is.
scanString :: Char -> Char -> Parser Builder
scanString start end = lexeme $ do
    s <- byteString <$ A8.char start <*> A8.scan False f <* A8.char end
    return (char8 start <> s <> char8 end)
  where
    f True  _       = Just False
    f False c
        | c == end  = Nothing
        | c == '\\' = Just True
        | otherwise = Just False

-- | Parse an email address, stripping out whitespace and comments.
addrSpec :: Parser B.ByteString
addrSpec = toByteString <$> (localPart <+> at <+> domain)
  where
    at            = char8 <$> symbol '@'
    dot           = char8 <$> symbol '.'
    dotSep p      = p <+> concatMany (dot <+> p)

    addrAtom      = byteString <$> atom
    addrQuote     = scanString '"' '"'
    domainLiteral = scanString '[' ']'

    localPart     = dotSep (addrAtom <|> addrQuote)
    domain        = dotSep addrAtom <|> domainLiteral

-- | Parse an address specification in angle brackets.
angleAddrSpec :: Parser B.ByteString
angleAddrSpec = symbol '<' *> addrSpec <* symbol '>'

-- | Parse two or more occurences of @p@, separated by @sep@.
sepBy2 :: Alternative f => f a -> f b -> f [a]
sepBy2 p sep = (:) <$> p <*> many1 (sep *> p)

-- | Parse a list of elements, with possibly null entries in between
-- separators. At least one entry or separator will be parsed.
optionalSepBy1 :: Alternative f => f a -> f b -> f [a]
optionalSepBy1 p sep = catMaybes <$> sepBy2 (optional p) sep
                   <|> return <$> p

-- | Parse a list of elements, separated by commas.
commaSep :: Parser a -> Parser [a]
commaSep p = optionalSepBy1 p (symbol ',')

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
    element = T.concat     <$> many1 (lexeme encodedWord)
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
    element = T.concat     <$> many1 (encodedWord <* fws)
          <|> decodeLatin1 <$> word <* fws

    word    = A.takeWhile1 (not . A8.isSpace_w8)

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

-- | Parse the content transfer encoding.
contentTransferEncoding :: Parser B.ByteString
contentTransferEncoding = token
