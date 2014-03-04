{-# LANGUAGE OverloadedStrings #-}
-- | Docs for header fields.
module Network.Email.Header.Render.Format
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
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Base64          as Base64
import           Data.ByteString.Lazy.Builder    (Builder)
import qualified Data.ByteString.Lazy.Builder    as B
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Time
import qualified Data.Text.Lazy                  as L
import           Data.Word
import           System.Locale

import           Network.Email.Charset
import           Network.Email.Header.Render.Doc
import           Network.Email.Layout            as F
import           Network.Email.Types

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

-- | Format a list of 'Recipient'es.
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
    toHexDigit d
        | d < 10    = B.word8 (d + 48)
        | otherwise = B.word8 (d + 55)

-- | Q-encode a word.
encodeQ :: Word8 -> (Int, Builder)
encodeQ w
    | w == 32   = (1, B.char8 '_')
    | isIllegal = (3, B.char8 '=' <> hex w)
    | otherwise = (1, B.word8 w)
  where
    isIllegal = w < 33 || w > 126 || w `B.elem` "()<>[]:;@\\\",?=_"

-- | Render a Q-encoded text.
renderQ :: B.ByteString -> Layout Builder
renderQ = B.foldr (\w l -> uncurry F.span (encodeQ w) <> l) mempty

-- | Split off Q-encoded text of a maximum length.
splitQ :: Int -> B.ByteString -> (Layout Builder, B.ByteString)
splitQ w b = fromMaybe (mempty, b) $ do
    (a, b') <- B.uncons b
    let (n, e)   = encodeQ a
        w'       = w - n
        (l, b'') = splitQ w' b'
    guard (w' >= 0)
    return (F.span n e <> l, b'')

-- | Render a base 64-encoded text.
renderBase64 :: B.ByteString -> Layout Builder
renderBase64 b = F.span (B.length e) (B.byteString e)
  where
    e = Base64.encode b

-- | Split off base 64-encoded text of a maximum length.
splitBase64 :: Int -> B.ByteString -> (Layout Builder, B.ByteString)
splitBase64 w b = (F.span (B.length e) (B.byteString e), B.drop n b)
  where
    n = 3*(w `div` 4)
    e = Base64.encode (B.take n b)

-- | Layout text as an encoded word.
layoutText :: RenderOptions -> Bool -> L.Text -> Layout Builder
layoutText r h t
    | h         = prefix <> whole e <> postfix
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

    (enc, whole, split) = case encoding r of
        QEncoding -> ("Q", renderQ, splitQ)
        Base64    -> ("B", renderBase64, splitBase64)

    splitLines b
        | B.null b  = mempty
        | otherwise = F.position $ \p ->
            let (l, b') = split (lineWidth r - p) b
            in  prefix <> l <> postfix <> newline r <> splitLines b'

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
