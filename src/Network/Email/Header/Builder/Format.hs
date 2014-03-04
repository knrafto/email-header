{-# LANGUAGE OverloadedStrings #-}
module Network.Email.Header.Builder.Format
    ( -- * Date and time
      dateTime
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

import           Control.Monad
import qualified Data.ByteString              as B
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy         as LB
import           Data.List                    (intersperse)
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Time
import qualified Data.Text.Lazy               as L
import qualified Data.Text.Lazy.Encoding      as L
import           Data.Time.LocalTime
import           Data.Word
import           System.Locale

import           Network.Email.Charset
import           Network.Email.Format         (Layout, Doc)
import qualified Network.Email.Format         as F
import           Network.Email.Types

infixr 6 </>

data Encoding = QEncoding | Base64
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

data RenderOptions = RenderOptions
    { lineWidth :: Int
    , indent    :: Int
    , converter :: Converter
    , encoding  :: Encoding
    } deriving (Eq)

-- | A header builder.
newtype Builder = Builder { runBuilder :: RenderOptions -> Doc B.Builder }

instance Monoid Builder where
    mempty      = Builder $ \_ -> mempty
    mappend a b = Builder $ \r -> runBuilder a r <> runBuilder b r

instance IsString Builder where
    fromString s = builder (length s) (B.string8 s)

-- | Construct a 'Builder' from a 'B.Builder'.
builder :: Int -> B.Builder -> Builder
builder k s = Builder $ \_ -> F.prim $ \_ -> F.span k s

-- | Construct a 'Builder' from a 'B.ByteString'.
byteString :: B.ByteString -> Builder
byteString s = builder (B.length s) (B.byteString s)

-- | Construct a 'Builder' from a 'L.Text'.
text :: L.Text -> Builder
text = byteString . LB.toStrict . L.encodeUtf8

-- | Group 'Builder's.
group :: Builder -> Builder
group a = Builder $ \r -> F.group (runBuilder a r)

-- | A space layout.
space :: Layout B.Builder
space = F.span 1 (B.char8 ' ')

-- | A newline layout.
newline :: RenderOptions -> Layout B.Builder
newline r =
    F.span 2 (B.byteString "\r\n") <>
    F.break 0 <>
    mconcat (replicate (indent r) space)

-- | A line break. If undone, behaves like a space.
line :: Builder
line = Builder $ \r -> F.prim $ \h -> if h then space else newline r

-- | A line break. If undone, behaves like `mempty`.
linebreak :: Builder
linebreak = Builder $ \r -> F.prim $ \h -> if h then mempty else newline r

-- | A line break or a space.
softline :: Builder
softline = group line

-- | A line break or `mempty`.
softbreak :: Builder
softbreak = group linebreak

-- | Concatenate with a 'softline' in between.
(</>) :: Builder -> Builder -> Builder
a </> b = a <> softline <> b

-- | Separate with lines or spaces.
sep :: [Builder] -> Builder
sep = group . mconcat . intersperse line

-- | Format an integer.
int :: Int -> Builder
int = fromString . show

-- | @punctuate p xs@ appends @p@ to every element of @xs@ but the last.
punctuate :: Monoid a => a -> [a] -> [a]
punctuate p = go
  where
    go []     = []
    go [x]    = [x]
    go (x:xs) = x <> p : xs

-- | Separate a group with commas.
commaSep :: (a -> Builder) -> [a] -> Builder
commaSep f = sep . punctuate "," . map f

-- | Format a date and time.
dateTime :: ZonedTime -> Builder
dateTime = fromString . formatTime defaultTimeLocale rfc822DateFormat

-- | Format an address.
address :: Address -> Builder
address (Address s) = byteString s

-- | Format an address with angle brackets.
angleAddr :: Address -> Builder
angleAddr a = "<" <> address a <> ">"

-- | Format a 'Mailbox'.
mailbox :: Mailbox -> Builder
mailbox (Mailbox n a) = case n of
    Nothing   -> address a
    Just name -> phrase name </> angleAddr a

-- | Format a list of 'Mailbox'es.
mailboxList :: [Mailbox] -> Builder
mailboxList = commaSep mailbox

-- | Format a 'Recipient'.
recipient :: Recipient -> Builder
recipient (Individual m)  = mailbox m
recipient (Group name ms) = phrase name <> ":" </> mailboxList ms

-- | Format a list of 'Recipient'es.
recipientList :: [Recipient] -> Builder
recipientList = commaSep recipient

-- | Format a message identifier
messageID :: MessageID -> Builder
messageID (MessageID s) = byteString s

-- | Format a list of message identifiers.
messageIDList :: [MessageID] -> Builder
messageIDList = commaSep messageID

-- | Convert a word to a hexadecimal value.
hex :: Word8 -> B.Builder
hex w = toHexDigit a <> toHexDigit b
  where
    (a, b)          = w `divMod` 16
    toHexDigit w
        | w < 10    = B.word8 (w + 48)
        | otherwise = B.word8 (w + 55)

-- | Q-encode a word.
encodeQ :: Word8 -> (Int, B.Builder)
encodeQ w
    | w == 32   = (1, B.char8 '_')
    | isIllegal = (3, B.char8 '=' <> hex w)
    | otherwise = (1, B.word8 w)
  where
    isIllegal = w < 33 || w > 126 || w `B.elem` "()<>[]:;@\\\",?=_"

-- | Render a Q-encoded text.
renderQ :: B.ByteString -> Layout B.Builder
renderQ = B.foldr (\w l -> uncurry F.span (encodeQ w) <> l) mempty

-- | Split off Q-encoded text of a maximum length.
splitQ :: Int -> B.ByteString -> (Layout B.Builder, B.ByteString)
splitQ w b = fromMaybe (mempty, b) $ do
    (a, b') <- B.uncons b
    let (n, e)   = encodeQ a
        w'       = w - n
        (l, b'') = splitQ w' b'
    guard (w' >= 0)
    return (F.span n e <> l, b'')

-- | Render a base 64-encoded text.
renderBase64 :: B.ByteString -> Layout B.Builder
renderBase64 b = F.span (B.length e) (B.byteString e)
  where
    e = Base64.encode b

-- | Split off base 64-encoded text of a maximum length.
splitBase64 :: Int -> B.ByteString -> (Layout B.Builder, B.ByteString)
splitBase64 w b = (F.span (B.length e) (B.byteString e), B.drop n b)
  where
    n = 3*(w `div` 4)
    e = Base64.encode (B.take n b)

-- | Layout text as an encoded word.
layoutText :: RenderOptions -> Bool -> L.Text -> Layout B.Builder
layoutText r h t
    | h         = prefix <> render e <> postfix
    | otherwise = splitLines e
  where
    e       = fromUnicode (converter r) (L.toStrict t)
    charset = getName (converter r)

    prefix  = F.span (5 + length charset) $
        B.byteString "?=" <>
        B.string8 charset <>
        B.char8 '?' <>
        B.byteString enc <>
        B.char8 '?'

    postfix = F.span 2 (B.byteString "?=")

    (enc, render, split) = case encoding r of
        QEncoding -> ("Q", renderQ, splitQ)
        Base64    -> ("B", renderBase64, splitBase64)

    splitLines b
        | B.null b  = mempty
        | otherwise = F.position $ \p ->
            let (l, b') = split (lineWidth r - p) b
            in  prefix <> l <> postfix <> newline r <> splitLines b'

-- | Encode text as an encoded word.
encodeText :: L.Text -> Builder
encodeText t = Builder $ \r -> F.prim $ \h -> layoutText r h t

-- | Encode text, given a predicate that checks for illegal characters.
renderText :: (Char -> Bool) -> L.Text -> Builder
renderText isIllegalChar t
    | mustEncode = encodeText t
    | otherwise  = sep (map text ws)
  where
    ws         = L.words t

    mustEncode = L.unwords ws /= t
              || any ("=?" `L.isPrefixOf`) ws
              || L.any isIllegalChar t

-- | Format a phrase. The text is encoded as is, unless:
-- * The text opens or closes with whitespace, or more than one space appears in
--   between words
-- * Any word begins with =?
-- * Any word contains illegal characters
phrase :: L.Text -> Builder
phrase = renderText (\c -> c > '~' || c < '!' || c `elem` "()<>[]:;@\\\",")

-- | Format a list of phrases.
phraseList :: [L.Text] -> Builder
phraseList = commaSep phrase

-- | Format unstructured text. The text is encoded as is, unless:
-- * The text opens or closes with whitespace, or more than one space appears in
--   between words
-- * Any word begins with =?
-- * Any word contains illegal characters
unstructured :: L.Text -> Builder
unstructured = renderText (\c -> c > '~' || c < '!')

-- | Format the MIME version.
mimeVersion ::  Int -> Int -> Builder
mimeVersion major minor = int major <> "." <> int minor

-- | Format the content type and parameters.
contentType :: MimeType -> Parameters -> Builder
contentType (MimeType t s) params = sep . punctuate ";" $
    renderMimeType : map renderParam (Map.toList params)
  where
    renderMimeType     = byteString t <> "/" <> byteString s
    renderParam (k, v) = byteString k <> "=" <> byteString v

-- | Format the content transfer encoding.
contentTransferEncoding :: B.ByteString -> Builder
contentTransferEncoding = byteString
