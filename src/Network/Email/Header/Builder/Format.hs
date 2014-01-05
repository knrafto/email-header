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

import qualified Data.ByteString      as B
import           Data.Monoid
import           Data.String
import qualified Data.Text.Lazy       as L
import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Locale

import           Network.Email.Format
import           Network.Email.Types

-- | Format an integer.
int :: Int -> Doc
int = fromString . show

commaSep :: (a -> Doc) -> [a] -> Doc
commaSep f = sep . punctuate "," . map f

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
recipient (Individual m ) = mailbox m
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

-- | Format a phrase.
phrase :: L.Text -> Doc
phrase = undefined

-- | Format a list of phrases.
phraseList :: [L.Text] -> Doc
phraseList = commaSep phrase

-- | Format unstructured text.
unstructured :: L.Text -> Doc
unstructured = undefined

-- | Format the MIME version.
mimeVersion ::  Int -> Int -> Doc
mimeVersion major minor = int major <> "." <> int minor

-- | Format the content type and parameters.
contentType :: MimeType -> Parameters -> Doc
contentType = undefined

-- | Format the content transfer encoding.
contentTransferEncoding :: B.ByteString -> Doc
contentTransferEncoding = undefined
