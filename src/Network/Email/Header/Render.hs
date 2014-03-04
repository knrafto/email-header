{-# LANGUAGE OverloadedStrings #-}
-- | Rendering common header fields.
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

import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy.Builder       as B
import qualified Data.CaseInsensitive               as CI
import qualified Data.Text.Lazy                     as L
import           Data.Time.LocalTime

import           Network.Email.Header.Render.Doc
import qualified Network.Email.Header.Render.Format as F
import           Network.Email.Types

-- | Render a list of headers.
renderHeaders :: RenderOptions -> [(HeaderName, Doc)] -> Headers
renderHeaders r = map (renderHeader r)

-- | Render a header.
renderHeader
    :: RenderOptions
    -> (HeaderName, Doc)
    -> (HeaderName, HeaderField)
renderHeader r (k, b) = (k, B.toLazyByteString l)
  where
    l = render r (B.length (CI.original k) + 2) b

-- | Build a header field.
buildField :: HeaderName -> (a -> Doc) -> a -> (HeaderName, Doc)
buildField k f a = (k, f a)

-- | Create a @Date:@ field.
date :: ZonedTime -> (HeaderName, Doc)
date = buildField "Date" F.dateTime

-- | Create a @From:@ field.
from :: [Mailbox] -> (HeaderName, Doc)
from = buildField "From" F.mailboxList

-- | Create a @Sender:@ field.
sender :: Mailbox -> (HeaderName, Doc)
sender = buildField "Sender" F.mailbox

-- | Create a @Reply-To:@ field.
replyTo :: [Recipient] -> (HeaderName, Doc)
replyTo = buildField "Reply-To" F.recipientList

-- | Create a @To:@ field.
to :: [Recipient] -> (HeaderName, Doc)
to = buildField "To" F.recipientList

-- | Create a @Cc:@ field.
cc :: [Recipient] -> (HeaderName, Doc)
cc = buildField "Cc" F.recipientList

-- | Create a @Bcc:@ field.
bcc :: Maybe [Recipient] -> (HeaderName, Doc)
bcc = buildField "Bcc" (optional F.recipientList)

-- | Create a @Message-ID:@ field.
messageID :: MessageID -> (HeaderName, Doc)
messageID = buildField "Message-ID" F.messageID

-- | Create a @In-Reply-To:@ field.
inReplyTo :: [MessageID] -> (HeaderName, Doc)
inReplyTo = buildField "In-Reply-To" (commaSep F.messageID)

-- | Create a @References:@ field.
references :: [MessageID] -> (HeaderName, Doc)
references = buildField "References" (commaSep F.messageID)

-- | Create a @Subject:@ field.
subject :: L.Text -> (HeaderName, Doc)
subject = buildField "Subject" F.unstructured

-- | Create a @comments:@ field.
comments :: L.Text -> (HeaderName, Doc)
comments = buildField "Comments" F.unstructured

-- | Create a @Keywords:@ field.
keywords :: [L.Text] -> (HeaderName, Doc)
keywords = buildField "Keywords" F.phraseList

-- | Create a @Resent-Date:@ field.
resentDate :: ZonedTime -> (HeaderName, Doc)
resentDate = buildField "Resent-Date" F.dateTime

-- | Create a @Resent-From:@ field.
resentFrom :: [Mailbox] -> (HeaderName, Doc)
resentFrom = buildField "Resent-From" F.mailboxList

-- | Create a @Resent-Sender:@ field.
resentSender :: Mailbox -> (HeaderName, Doc)
resentSender = buildField "Resent-Sender" F.mailbox

-- | Create a @Resent-To:@ field.
resentTo :: [Recipient] -> (HeaderName, Doc)
resentTo = buildField "Resent-To" F.recipientList

-- | Create a @Resent-Cc:@ field.
resentCc :: [Recipient] -> (HeaderName, Doc)
resentCc = buildField "Resent-Cc" F.recipientList

-- | Create a @Resent-Bcc:@ field.
resentBcc :: Maybe [Recipient] -> (HeaderName, Doc)
resentBcc = buildField "Resent-Bcc" (optional F.recipientList)

-- | Create a @Resent-Message-ID:@ field.
resentMessageID :: MessageID -> (HeaderName, Doc)
resentMessageID = buildField "Resent-Message-ID" F.messageID

-- | Create a @MIME-Version:@ field.
mimeVersion :: Int -> Int -> (HeaderName, Doc)
mimeVersion major minor = ("MIME-Version", F.mimeVersion major minor)

-- | Create a @Content-Type:@ field.
contentType :: MimeType -> Parameters -> (HeaderName, Doc)
contentType t params = ("Content-Type", F.contentType t params)

-- | Create a @Content-Transfer-Encoding:@ field.
contentTransferEncoding :: B.ByteString -> (HeaderName, Doc)
contentTransferEncoding =
    buildField "Content-Transfer-Encoding" F.contentTransferEncoding

-- | Create a @Content-ID:@ field.
contentID :: MessageID -> (HeaderName, Doc)
contentID = buildField "Content-ID" F.messageID
