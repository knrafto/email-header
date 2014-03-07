{-# LANGUAGE OverloadedStrings #-}
-- | Docs for header fields.
module Network.Email.Header.Render.Internal
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

import           Control.Arrow
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Base64             as Base64
import           Data.ByteString.Lazy.Builder       (Builder)
import qualified Data.ByteString.Lazy.Builder       as B
import           Data.Char
import qualified Data.Map                           as Map
import           Data.Monoid
import           Data.String
import           Data.Time
import           Data.Text.ICU.Convert
import qualified Data.Text.Lazy                     as L
import           Data.Word
import           System.Locale

import           Network.Email.Header.Render.Doc
import           Network.Email.Header.Render.Layout as F
import           Network.Email.Header.Types

-- | Format a date and time.
dateTime :: ZonedTime -> Doc
dateTime = fromString . formatTime defaultTimeLocale rfc822DateFormat

-- | Format an address.
address :: Address -> Doc
address (Address s) = byteString s

-- | Format an address with angle brackets.
angleAddr :: Address -> Doc
angleAddr a = "<" <> address a <> ">"

-- | Format a 'Mailbox'.
mailbox :: Mailbox -> Doc
mailbox (Mailbox n a) = case n of
    Nothing   -> address a
    Just name -> phrase name </> angleAddr a

-- | Format a list of 'Mailbox'es.
mailboxList :: [Mailbox] -> Doc
mailboxList = commaSep mailbox

-- | Format a 'Recipient'.
recipient :: Recipient -> Doc
recipient (Individual m)  = mailbox m
recipient (Group name ms) = phrase name <> ":" </> mailboxList ms

-- | Format a list of 'Recipient's.
recipientList :: [Recipient] -> Doc
recipientList = commaSep recipient

-- | Format a message identifier
messageID :: MessageID -> Doc
messageID (MessageID s) = byteString s

-- | Format a list of message identifiers.
messageIDList :: [MessageID] -> Doc
messageIDList = commaSep messageID

-- | Convert a word to a hexadecimal value.
hex :: Word8 -> Builder
hex w = toHexDigit a <> toHexDigit b
  where
    (a, b)          = w `divMod` 16
    toHexDigit n
        | n < 10    = B.word8 (n + 48)
        | otherwise = B.word8 (n + 55)

-- | Encode a word.
encodeWord :: RenderOptions -> L.Text -> (Int, Builder)
encodeWord r = encodeWith (encoding r) . fromUnicode (converter r) . L.toStrict
  where
    encodeWith QEncoding = encodeQ
    encodeWith Base64    = encodeBase64

    encodeQ              = first getSum .
                           B.foldr (\w a -> encodeWord8 w <> a) mempty

    encodeWord8 w
        | w == 32        = (Sum 1, B.char8 '_')
        | isIllegal w    = (Sum 3, B.char8 '=' <> hex w)
        | otherwise      = (Sum 1, B.word8 w)

    isIllegal w          = w < 33
                        || w > 126
                        || w `B.elem` "()<>[]:;@\\\",?=_"

    encodeBase64 b       = let e = Base64.encode b
                           in  (B.length e, B.byteString e)

-- | Split nonempty text into a layout that fits the given width and the
-- remainder.
-- TODO: inefficient
splitWord :: RenderOptions -> Int -> L.Text -> (Layout Builder, L.Text)
splitWord r w t =
    first (uncurry F.span) .
    last .
    takeWhile1 (fits . fst) .
    map (first (encodeWord r)) .
    drop 1 $
    zip (L.inits t) (L.tails t)
  where
    fits (l, _) = l <= w

    takeWhile1 _ []     = []
    takeWhile1 p (x:xs) = x : takeWhile p xs

-- | Layout text as an encoded word.
layoutText :: RenderOptions -> Bool -> L.Text -> Layout Builder
layoutText r h t0
    | L.null t0 = mempty
    | h         = prefix <> uncurry F.span (encodeWord r t0) <> postfix
    | otherwise = splitLines t0
  where
    charset = map toLower . getName $ converter r

    method  = case encoding r of
        QEncoding -> 'Q'
        Base64    -> 'B'

    prefix  = F.span (5 + length charset) $
        B.byteString "=?" <>
        B.string8 charset <>
        B.char8 '?' <>
        B.char8 method <>
        B.char8 '?'

    postfix = F.span 2 (B.byteString "?=")

    padding = 7 + length charset

    splitLines t = F.position $ \p ->
        let (l, t') = splitWord r (lineWidth r - padding - p) t
        in  prefix <> l <> postfix <>
            (if L.null t' then mempty else newline r <> splitLines t')

-- | Encode text as an encoded word.
encodeText :: L.Text -> Doc
encodeText t = prim $ \r h -> layoutText r h t

-- | Encode text, given a predicate that checks for illegal characters.
renderText :: (Char -> Bool) -> L.Text -> Doc
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
phrase :: L.Text -> Doc
phrase = renderText (\c -> c > '~' || c < '!' || c `elem` "()<>[]:;@\\\",")

-- | Format a list of phrases.
phraseList :: [L.Text] -> Doc
phraseList = commaSep phrase

-- | Format unstructured text. The text is encoded as is, unless:
-- * The text opens or closes with whitespace, or more than one space appears in
--   between words
-- * Any word begins with =?
-- * Any word contains illegal characters
unstructured :: L.Text -> Doc
unstructured = renderText (\c -> c > '~' || c < '!')

-- | Format the MIME version.
mimeVersion ::  Int -> Int -> Doc
mimeVersion major minor = int major <> "." <> int minor
  where
    int = fromString . show

-- | Format the content type and parameters.
contentType :: MimeType -> Parameters -> Doc
contentType (MimeType t s) params = sep . punctuate ";" $
    renderMimeType : map renderParam (Map.toList params)
  where
    renderMimeType     = byteString t <> "/" <> byteString s
    renderParam (k, v) = byteString k <> "=" <> byteString v

-- | Format the content transfer encoding.
contentTransferEncoding :: B.ByteString -> Doc
contentTransferEncoding = byteString
