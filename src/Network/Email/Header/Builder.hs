{-# LANGUAGE OverloadedStrings #-}
-- | Building common header fields.
module Network.Email.Header.Builder
    ( -- * Rendering
      RenderOptions(..)
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

import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy.Builder        as B
import qualified Data.CaseInsensitive                as CI
import qualified Data.Text.Lazy                      as L
import           Data.Time.LocalTime

import           Network.Email.Header.Builder.Format (Builder, RenderOptions, render)
import qualified Network.Email.Header.Builder.Format as F
import           Network.Email.Format                (layout)
import           Network.Email.Types

-- | Render a list of headers.
renderHeaders :: RenderOptions -> [(HeaderName, Builder)] -> Headers
renderHeaders r = map (renderHeader r)

-- | Render a header.
renderHeader
    :: RenderOptions
    -> (HeaderName, Builder)
    -> (HeaderName, HeaderField)
renderHeader r (k, b) = (k, B.toLazyByteString l)
  where
    l = layout (render r b) (B.length (CI.original k) + 2)

-- | Build a header field.
buildField :: HeaderName -> (a -> Builder) -> a -> (HeaderName, Builder)
buildField k f a = (k, f a)

-- | Create a @Date:@ field.
date :: ZonedTime -> (HeaderName, Builder)
date = buildField "Date" F.dateTime

-- | Create a @From:@ field.
from :: [Mailbox] -> (HeaderName, Builder)
from = buildField "From" F.mailboxList

-- | Create a @Sender:@ field.
sender :: Mailbox -> (HeaderName, Builder)
sender = buildField "Sender" F.mailbox

-- | Create a @Reply-To:@ field.
replyTo :: [Recipient] -> (HeaderName, Builder)
replyTo = buildField "Reply-To" F.recipientList

-- | Create a @To:@ field.
to :: [Recipient] -> (HeaderName, Builder)
to = buildField "To" F.recipientList

-- | Create a @Cc:@ field.
cc :: [Recipient] -> (HeaderName, Builder)
cc = buildField "Cc" F.recipientList

-- | Create a @Bcc:@ field.
bcc :: Maybe [Recipient] -> (HeaderName, Builder)
bcc = buildField "Bcc" (F.optional F.recipientList)

-- | Create a @Message-ID:@ field.
messageID :: MessageID -> (HeaderName, Builder)
messageID = buildField "Message-ID" F.messageID

-- | Create a @In-Reply-To:@ field.
inReplyTo :: [MessageID] -> (HeaderName, Builder)
inReplyTo = buildField "In-Reply-To" (F.commaSep F.messageID)

-- | Create a @References:@ field.
references :: [MessageID] -> (HeaderName, Builder)
references = buildField "References" (F.commaSep F.messageID)

-- | Create a @Subject:@ field.
subject :: L.Text -> (HeaderName, Builder)
subject = buildField "Subject" F.unstructured

-- | Create a @comments:@ field.
comments :: L.Text -> (HeaderName, Builder)
comments = buildField "Comments" F.unstructured

-- | Create a @Keywords:@ field.
keywords :: [L.Text] -> (HeaderName, Builder)
keywords = buildField "Keywords" F.phraseList

-- | Create a @Resent-Date:@ field.
resentDate :: ZonedTime -> (HeaderName, Builder)
resentDate = buildField "Resent-Date" F.dateTime

-- | Create a @Resent-From:@ field.
resentFrom :: [Mailbox] -> (HeaderName, Builder)
resentFrom = buildField "Resent-From" F.mailboxList

-- | Create a @Resent-Sender:@ field.
resentSender :: Mailbox -> (HeaderName, Builder)
resentSender = buildField "Resent-Sender" F.mailbox

-- | Create a @Resent-To:@ field.
resentTo :: [Recipient] -> (HeaderName, Builder)
resentTo = buildField "Resent-To" F.recipientList

-- | Create a @Resent-Cc:@ field.
resentCc :: [Recipient] -> (HeaderName, Builder)
resentCc = buildField "Resent-Cc" F.recipientList

-- | Create a @Resent-Bcc:@ field.
resentBcc :: Maybe [Recipient] -> (HeaderName, Builder)
resentBcc = buildField "Resent-Bcc" (F.optional F.recipientList)

-- | Create a @Resent-Message-ID:@ field.
resentMessageID :: MessageID -> (HeaderName, Builder)
resentMessageID = buildField "Resent-Message-ID" F.messageID

-- | Create a @MIME-Version:@ field.
mimeVersion :: Int -> Int -> (HeaderName, Builder)
mimeVersion major minor = ("MIME-Version", F.mimeVersion major minor)

-- | Create a @Content-Type:@ field.
contentType :: MimeType -> Parameters -> (HeaderName, Builder)
contentType t params = ("Content-Type", F.contentType t params)

-- | Create a @Content-Transfer-Encoding:@ field.
contentTransferEncoding :: B.ByteString -> (HeaderName, Builder)
contentTransferEncoding =
    buildField "Content-Transfer-Encoding" F.contentTransferEncoding

-- | Create a @Content-ID:@ field.
contentID :: MessageID -> (HeaderName, Builder)
contentID = buildField "Content-ID" F.messageID
