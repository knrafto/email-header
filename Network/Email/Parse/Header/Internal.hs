{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Parsing of basic header syntax, and helper functions.
module Network.Email.Parse.Header.Internal
    ( -- * Monad failure
      failNothing
    , failLeft
      -- * Whitespace
    , fws
    , cfws
    , padded
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
import           Control.Monad
import           Data.Attoparsec              (Parser)
import qualified Data.Attoparsec              as A
import qualified Data.Attoparsec.Char8        as A8
import qualified Data.Attoparsec.Zepto        as Z
import           Data.Bits
import qualified Data.ByteString              as B
import qualified Data.ByteString.Base64       as Base64
import           Data.ByteString.Internal     (w2c)
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as L
import qualified Data.ByteString.Unsafe       as B
import           Data.List
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Word

import qualified Network.Email.Charset as CS

-- | Return a 'Just' value, and 'fail' a 'Nothing' value.
failNothing :: Monad m => String -> Maybe a -> m a
failNothing s = maybe (fail s) return

-- | Return a 'Right' value, and 'fail' a 'Left' value.
failLeft :: Monad m => Either String a -> m a
failLeft = either fail return

-- | Run a 'Builder' as a strict 'B.ByteString'.
toByteString :: Builder -> B.ByteString
toByteString = L.toStrict . toLazyByteString

-- | Skip folding whitespace.
fws :: Parser ()
fws = A8.skipSpace

-- | Parse a comment, including all nested comments.
comment :: Parser B.ByteString
comment = A8.char '(' *> A.scan (1 :: Int, False) f <* A8.char ')'
  where
    f (!n, True ) _ = Just (n, False)
    f (!n, False) w = case w2c w of
        '('  -> Just (n + 1, False)
        ')'  -> if n == 1 then Nothing else Just (n - 1, False)
        '\\' -> Just (n, True)
        _    -> Just (n, False)

-- | Skip any comments or folding whitespace.
cfws :: Parser ()
cfws = fws <* optional (comment *> cfws)

-- | Parse a value, surrounded by whitespace.
padded :: Parser a -> Parser a
padded p = cfws *> p <* cfws

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

-- | Parse an token lexeme consisting of all printable characters, but
--  disallowing the specified special characters.
tokenWith :: String -> Parser B.ByteString
tokenWith specials = A.takeWhile1 isAtom
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
textToken = A.takeWhile1 (not . A8.isSpace_w8)

-- | Parse an escaped 'B.ByteString'.
unescape
    :: (Char -> Bool)    -- ^ Detect a possible escape sequence.
    -> Z.Parser Builder  -- ^ Escape sequence parser.
    -> B.ByteString      -- ^ Input 'B.ByteString'.
    -> Either String B.ByteString
unescape p decode str
    | B.any p' str = Z.parse (toByteString <$> go mempty) str
    | otherwise    = return str
  where
    p' = p . w2c

    go acc = do
        s    <- Z.takeWhile (not . p')
        done <- Z.atEnd
        if done
            then return (acc <> byteString s)
            else do
                b <- decode
                go (acc <> byteString s <> b)

-- | Scan for the end of a string.
scanString :: Char -> Parser B.ByteString
scanString end = A.scan False f
  where
    f True _ = Just False
    f _    w = case w2c w of
        c | c == end -> Nothing
        '\\'         -> Just True
        _            -> Just False

-- | Parse a raw (undecoded) quoted-string.
rawQuotedString :: Parser B.ByteString
rawQuotedString = A8.char '"' *> scanString '"' <* A8.char '"'

-- | Parse a quoted-string.
quotedString :: Parser B.ByteString
quotedString = do
    s <- rawQuotedString
    failLeft $ unescape (\c -> c == '\\' || c == '\r') decode s
  where
    decode = decodeEscape <$> Z.take 2
    decodeEscape s
        | s == "\r\n" = mempty
        | otherwise   = word8 (B.unsafeIndex s 1)

-- | Parse an encoded word, as per RFC 2047.
encodedWord :: Parser T.Text
encodedWord = do
    _       <- A.string "=?"
    charset <- B8.unpack <$> tokenWith "()<>@,;:\"/[]?.="
    _       <- A8.char '?'
    method  <- decodeQuoted  <$ A.satisfy (`B.elem` "Qq") <|>
               Base64.decode <$ A.satisfy (`B.elem` "Bb")
    _       <- A8.char '?'
    enc     <- A8.takeWhile (/= '?')
    _       <- A.string "?="
    CS.decode charset <$> failLeft (method enc)
  where
    decodeQuoted = unescape (\c -> c == '=' || c == '_') decode
      where
        decode    = unquote . B.unsafeHead =<< Z.take 1
        unquote w = case w2c w of
            '_' -> return $ char8 ' '
            '=' -> word8 <$> hexPair
            _   -> error "unquote: impossible case"

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
    sequence [localPart, char8 '@' <$ padded (A8.char '@'), domain]
  where
    wrap c b  = char8 c <> b <> char8 c
    dotSep p  = mconcat . intersperse (char8 '.') <$>
                A.sepBy1 p (padded $ A8.char '.')

    addrAtom  = byteString <$> atom
    addrQuote = wrap '"' . byteString <$> rawQuotedString

    localPart = dotSep (addrAtom <|> addrQuote)
    domain    = dotSep addrAtom <|> domainLiteral

-- | Parse an address specification in angle brackets.
angleAddrSpec :: Parser B.ByteString
angleAddrSpec = A8.char '<' *> padded addrSpec <* A8.char '>'

-- | Parse a domain literal.
domainLiteral :: Parser Builder
domainLiteral = wrap <$ A8.char '[' <*> scanString ']' <* A8.char8 ']'
  where
    wrap s = char8 '[' <> byteString s <> char8 ']'

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
commaSep p = optionalSepBy p (padded $ A8.char ',')
