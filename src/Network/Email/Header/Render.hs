{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Rendering common header fields.
-- This module is intended to be imported qualified:
--
-- > import qualified Network.Email.Header.Render as R
module Network.Email.Header.Render
    ( -- * Rendering options
      RenderOptions(..)
    , Encoding(..)
    , defaultRenderOptions
      -- * Rendering
    , Doc
    , renderHeaders
      -- * Origination date field
    , date
      -- * Originator fields
    , from
    , sender
    , replyTo
      -- * Destination address fields
    , to
    , cc
    , bcc
      -- * Identification fields
    , messageID
    , inReplyTo
    , references
      -- * Informational fields
    , subject
    , comments
    , keywords
      -- * Resent fields
    , resentDate
    , resentFrom
    , resentSender
    , resentTo
    , resentCc
    , resentBcc
    , resentMessageID
      -- * MIME fields
    , mimeVersion
    , contentType
    , contentTransferEncoding
    , contentID
    ) where

import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy.Builder as B
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
#if MIN_VERSION_base(4,8,0)
#else
import           Data.Monoid
#endif
import qualified Data.Text.Lazy               as L
import           Data.Time.LocalTime

import           Network.Email.Header.Doc
import qualified Network.Email.Header.Pretty  as P
import           Network.Email.Header.Types

-- | Render a list of headers.
renderHeaders :: RenderOptions -> [(HeaderName, Doc)] -> Headers
renderHeaders r = map (renderHeader r)

-- | Render a header.
renderHeader :: RenderOptions -> (HeaderName, Doc) -> Header
renderHeader r (k, b) = (k, B.toLazyByteString l)
  where
    l = render r (B.length (CI.original k) + 2) b

-- | Build a header field.
buildField :: HeaderName -> (a -> Doc) -> a -> (HeaderName, Doc)
buildField k f a = (k, f a)

-- | Create a @Date:@ field.
date :: ZonedTime -> (HeaderName, Doc)
date = buildField "Date" P.dateTime

-- | Create a @From:@ field.
from :: [Mailbox] -> (HeaderName, Doc)
from = buildField "From" P.mailboxList

-- | Create a @Sender:@ field.
sender :: Mailbox -> (HeaderName, Doc)
sender = buildField "Sender" P.mailbox

-- | Create a @Reply-To:@ field.
replyTo :: [Recipient] -> (HeaderName, Doc)
replyTo = buildField "Reply-To" P.recipientList

-- | Create a @To:@ field.
to :: [Recipient] -> (HeaderName, Doc)
to = buildField "To" P.recipientList

-- | Create a @Cc:@ field.
cc :: [Recipient] -> (HeaderName, Doc)
cc = buildField "Cc" P.recipientList

-- | Create a @Bcc:@ field.
bcc :: Maybe [Recipient] -> (HeaderName, Doc)
bcc = buildField "Bcc" (maybe mempty P.recipientList)

-- | Create a @Message-ID:@ field.
messageID :: MessageID -> (HeaderName, Doc)
messageID = buildField "Message-ID" P.messageID

-- | Create a @In-Reply-To:@ field.
inReplyTo :: [MessageID] -> (HeaderName, Doc)
inReplyTo = buildField "In-Reply-To" (sep . map P.messageID)

-- | Create a @References:@ field.
references :: [MessageID] -> (HeaderName, Doc)
references = buildField "References" (sep . map P.messageID)

-- | Create a @Subject:@ field.
subject :: L.Text -> (HeaderName, Doc)
subject = buildField "Subject" P.unstructured

-- | Create a @Comments:@ field.
comments :: L.Text -> (HeaderName, Doc)
comments = buildField "Comments" P.unstructured

-- | Create a @Keywords:@ field.
keywords :: [L.Text] -> (HeaderName, Doc)
keywords = buildField "Keywords" P.phraseList

-- | Create a @Resent-Date:@ field.
resentDate :: ZonedTime -> (HeaderName, Doc)
resentDate = buildField "Resent-Date" P.dateTime

-- | Create a @Resent-From:@ field.
resentFrom :: [Mailbox] -> (HeaderName, Doc)
resentFrom = buildField "Resent-From" P.mailboxList

-- | Create a @Resent-Sender:@ field.
resentSender :: Mailbox -> (HeaderName, Doc)
resentSender = buildField "Resent-Sender" P.mailbox

-- | Create a @Resent-To:@ field.
resentTo :: [Recipient] -> (HeaderName, Doc)
resentTo = buildField "Resent-To" P.recipientList

-- | Create a @Resent-Cc:@ field.
resentCc :: [Recipient] -> (HeaderName, Doc)
resentCc = buildField "Resent-Cc" P.recipientList

-- | Create a @Resent-Bcc:@ field.
resentBcc :: Maybe [Recipient] -> (HeaderName, Doc)
resentBcc = buildField "Resent-Bcc" (maybe mempty P.recipientList)

-- | Create a @Resent-Message-ID:@ field.
resentMessageID :: MessageID -> (HeaderName, Doc)
resentMessageID = buildField "Resent-Message-ID" P.messageID

-- | Create a @MIME-Version:@ field.
mimeVersion :: Int -> Int -> (HeaderName, Doc)
mimeVersion major minor = ("MIME-Version", P.mimeVersion major minor)

-- | Create a @Content-Type:@ field.
contentType :: MimeType -> Parameters -> (HeaderName, Doc)
contentType t params = ("Content-Type", P.contentType t params)

-- | Create a @Content-Transfer-Encoding:@ field.
contentTransferEncoding :: CI B.ByteString -> (HeaderName, Doc)
contentTransferEncoding =
    buildField "Content-Transfer-Encoding" P.contentTransferEncoding

-- | Create a @Content-ID:@ field.
contentID :: MessageID -> (HeaderName, Doc)
contentID = buildField "Content-ID" P.messageID
