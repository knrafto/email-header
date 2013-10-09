{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Parsing of basic header syntax, and helper functions.
module Network.Email.Parse.Internal
    ( -- * Monad failure
      failNothing
    , failLeft
      -- * Whitespace
    , skipFws
    , skipCfws
    , lexeme
    , symbol
    , character
      -- * Numbers
    , digits
      -- * Atoms
    , atom
    , dotAtom
    , token
    , textToken
    , quotedString
    , encodedWord
      -- * Address specification
    , addrSpec
    , angleAddrSpec
      -- * Combinators
    , commaSep
    ) where

import           Control.Applicative
import           Data.Attoparsec         (Parser)
import qualified Data.Attoparsec         as A
import qualified Data.Attoparsec.Char8   as A8
import qualified Data.Attoparsec.Zepto   as Z
import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Base64  as Base64
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8   as B8
import qualified Data.ByteString.Lazy    as L
import qualified Data.ByteString.Unsafe  as B
import           Data.List
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Word

import qualified Network.Email.Charset   as CS

-- | Return a 'Just' value, and fail a 'Nothing' value.
failNothing :: Monad m => String -> Maybe a -> m a
failNothing s = maybe (fail s) return

-- | Return a 'Right' value, and fail a 'Left' value.
failLeft :: Monad m => Either String a -> m a
failLeft = either fail return

-- | Run a 'Builder' as a strict 'B.ByteString'.
toByteString :: Builder -> B.ByteString
toByteString = L.toStrict . toLazyByteString

-- | Skip folding whitespace.
skipFws :: Parser ()
skipFws = A8.skipSpace

-- | Parse a comment, including all nested comments.
comment :: Parser B.ByteString
comment =
    A.word8 openParen *> A.scan (1 :: Int, False) f <* A.word8 closeParen
  where
    openParen  = 40
    closeParen = 41
    backslash  = 92

    f (!n, True ) _        = Just (n, False)
    f (!n, False) c
        | c == closeParen  = if n == 1 then Nothing else Just (n - 1, False)
        | c == openParen   = Just (n + 1, False)
        | c == backslash   = Just (n, True)
        | otherwise        = Just (n, False)

-- | Skip any comments or folding whitespace.
skipCfws :: Parser ()
skipCfws = skipFws *> (comment *> skipCfws <|> return ())

-- | Skip comments or white space after a parser.
lexeme :: Parser a -> Parser a
lexeme = (<* skipCfws)

-- | Skip comments or white space after a string.
symbol :: B.ByteString -> Parser B.ByteString
symbol = lexeme . A.string

-- | Skip comments or wuite space after a character.
character :: Word8 -> Parser Word8
character = lexeme . A.word8

-- | Quickly (and unsafely) convert a digit to the number it represents.
fromDigit :: Integral a => Word8 -> a
fromDigit w = fromIntegral (w - 48)

-- Parse a set number of digits.
digits :: Integral a => Int -> Parser a
digits 0 = return 0
digits 1 = fromDigit <$> A.satisfy A8.isDigit_w8
digits n = do
    s <- A.take n
    unless (B.all A8.isDigit_w8 s) $
        fail $ "expected " ++ show n ++ " digits"
    return $ B.foldl' step 0 s
  where
    step a w = a * 10 + fromDigit w

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

-- | A raw, non-space token.
textToken :: Parser B.ByteString
textToken = lexeme (A.takeWhile1 (not . A8.isSpace_w8))

-- | Parse an escaped 'B.ByteString'.
unescape
    :: (Word8 -> Bool)   -- ^ Detect a possible escape sequence.
    -> Z.Parser Builder  -- ^ Escape sequence parser.
    -> B.ByteString      -- ^ Input 'B.ByteString'.
    -> Either String B.ByteString
unescape p decode str
    | B.any p str = Z.parse (toByteString <$> go mempty) str
    | otherwise   = return str
  where
    go acc       = do
        s    <- Z.takeWhile (not . p)
        done <- Z.atEnd
        if done
            then return (acc <> byteString s)
            else do
                b <- decode
                go (acc <> byteString s <> b)

-- | Scan for the end of a string.
scanString :: Word8 -> Parser B.ByteString
scanString end = A.scan False f
  where
    backslash = 92

    f True _             = Just False
    f _    c
        | c == end       = Nothing
        | c == backslash = Just True
        | otherwise      = Just False

-- | Parse a raw (undecoded) quoted-string.
rawQuotedString :: Parser B.ByteString
rawQuotedString = lexeme (A.word8 doubleQuote *> qtext <* A.word8 doubleQuote)
  where
    doubleQuote = 34
    qtext       = scanString doubleQuote

-- | Parse a quoted-string.
quotedString :: Parser B.ByteString
quotedString = do
    s <- rawQuotedString
    failLeft $ unescape isEscape decode s
  where
    carriageReturn = 13
    backslash      = 92
    isEscape w     = w == backslash || w == carriageReturn

    decode         = do
        s <- Z.take 2
        return $ if s == "\r\n"
                 then mempty
                 else word8 (B.unsafeIndex s 1)

-- | Parse an encoded word, as per RFC 2047.
encodedWord :: Parser T.Text
encodedWord = lexeme $ do
    _       <- A.string "=?"
    charset <- B8.unpack <$> A.takeWhile isToken
    _       <- A.word8 questionMark
    method  <- decodeQuoted  <$ A.satisfy (`B.elem` "Qq") <|>
                   Base64.decode <$ A.satisfy (`B.elem` "Bb")
    _       <- A.word8 questionMark
    enc     <- A.takeWhile (/= questionMark)
    _       <- A.string "?="
    CS.decode charset <$> failLeft (method enc)
  where
    spaceChar    = 32
    equals       = 61
    questionMark = 63
    underscore   = 95
    isToken w    = w <= 127 &&
                   w >= 33 &&
                   A.notInClass "()<>@,;:\"/[]?.=" w

    decodeQuoted = unescape (\c -> c == equals || c == underscore) decode
      where
        decode                = unquote . B.unsafeHead =<< Z.take 1
        unquote e
            | e == underscore = return $ word8 spaceChar
            | e == equals     = word8 <$> hexPair
            | otherwise       = error "unquote: the impossible happened!"

-- | Parse a 'Word8' represented as a pair of hex digits.
hexPair :: Z.Parser Word8
hexPair = do
    s <- Z.take 2
    let !a = hex (B.unsafeHead s)
        !b = hex (B.unsafeIndex s 1)
    if a .|. b /= 255
        then return $! (a `shiftL` 4) .|. b
        else fail "invalid hex escape"
  where
    hex w | w >= 48 && w <= 57 = w - 48
          | w >= 65 && w <= 90 = w - 55
          | otherwise          = 255

-- | Parse an email address, stripping out whitespace and comments.
addrSpec :: Parser B.ByteString
addrSpec = toByteString . mconcat <$>
    sequence [localPart, word8 atSign <$ character atSign, domain]
  where
    doubleQuote = 34
    dot         = 46
    atSign      = 64

    wrap w b    = word8 w <> b <> word8 w
    dotSep p    = mconcat . intersperse (word8 dot) <$>
                  A.sepBy1 p (character dot)

    addrAtom    = byteString <$> atom
    addrQuote   = wrap doubleQuote . byteString <$> rawQuotedString

    localPart   = dotSep (addrAtom <|> addrQuote)
    domain      = dotSep addrAtom <|> domainLiteral

-- | Parse an address specification in angle brackets.
angleAddrSpec :: Parser B.ByteString
angleAddrSpec = character leftAngle *> addrSpec <* character rightAngle
  where
    leftAngle  = 60
    rightAngle = 62

-- | Parse a domain literal.
domainLiteral :: Parser Builder
domainLiteral = lexeme $ do
    s <- A.word8 leftBracket *> dtext <* A.word8 rightBracket
    return $ word8 leftBracket <> byteString s <> word8 rightBracket
  where
    leftBracket  = 91
    rightBracket = 93
    dtext        = scanString rightBracket

-- | @optionalSepBy p sep@ applies /zero/ or more occurrences of @p@,
-- separated by @sep@. Null entries between separators will be filtered
-- out.
optionalSepBy :: Alternative f => f a -> f s -> f [a]
optionalSepBy p sep = step
  where
    step  = sep *> step
        <|> (:) <$> p <*> (sep *> step <|> pure [])
        <|> pure []

-- | Parse a list of elements, separated by commas.
commaSep :: Parser a -> Parser [a]
commaSep p = optionalSepBy p (character comma)
  where
    comma = 44
