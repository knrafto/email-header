{-# LANGUAGE BangPatterns, OverloadedStrings #-}
-- | Parsing of basic header syntax, and helper functions.
module Network.Email.Header.Parse.Internal
    ( -- * Monad failure
      parseMaybe
    , parseEither
      -- * Whitespace
    , fws
    , cfws
    , lexeme
    , symbol
      -- * Numbers
    , digits
    , number
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
import           Data.Attoparsec.Combinator
import           Data.Bits
import qualified Data.ByteString              as B
import qualified Data.ByteString.Base64       as Base64
import           Data.ByteString.Internal     (w2c)
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as L
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Word

import qualified Network.Email.Charset        as CS

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
        | w >= 64   = w - 54
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

-- | A raw, non-space token.
textToken :: Parser B.ByteString
textToken = lexeme $ A.takeWhile1 (not . A8.isSpace_w8)

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
encodedWord = lexeme $ do
    _       <- A.string "=?"
    charset <- B8.unpack <$> tokenWith "()<>@,;:\"/[]?.="
    _       <- A8.char '?'
    method  <- decodeMethod
    _       <- A8.char '?'
    enc     <- CS.decode charset <$> method
    _       <- A.string "?="
    return enc
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

-- | @optionalSepBy p sep@ applies /zero/ or more occurrences of @p@,
-- separated by @sep@. Null entries between separators will be filtered
-- out.
optionalSepBy :: Alternative f => f a -> f s -> f [a]
optionalSepBy p sep = step
  where
    step = sep *> step
       <|> (:) <$> p <*> end
       <|> pure []

    end  = sep *> step
       <|> pure []

-- | Parse a list of elements, separated by commas.
commaSep :: Parser a -> Parser [a]
commaSep p = optionalSepBy p (symbol ',')
