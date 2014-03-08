{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of common header fields.
-- This module is intended to be imported qualified.
module Network.Email.Header.Parse
    ( -- * Parsing
      field
    , structuredField
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

import           Control.Applicative
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Lazy
import qualified Data.ByteString                     as B
import           Data.CaseInsensitive                (CI)
import qualified Data.Text.Lazy                      as L
import           Data.Time.LocalTime

import qualified Network.Email.Header.Parse.Internal as P
import           Network.Email.Header.Types

-- | Lookup and parse a header with a parser.
field :: HeaderName -> Parser a -> Headers -> Maybe a
field k p hs = do
    body <- lookup k hs
    maybeResult $ parse p body

-- | Lookup and parse a structured header with a parser. This skips initial
-- comments and folding white space, and ensures that the entire body is
-- consumed by the parser.
structuredField :: HeaderName -> Parser a -> Headers -> Maybe a
structuredField k p = field k (P.cfws *> p <* endOfInput)

-- | Get the value of the @Date:@ field.
date :: Headers -> Maybe ZonedTime
date = structuredField "Date" P.dateTime

-- | Get the value of the @From:@ field.
from :: Headers -> Maybe [Mailbox]
from = structuredField "From" P.mailboxList

-- | Get the value of the @Sender:@ field.
sender :: Headers -> Maybe Mailbox
sender = structuredField "Sender" P.mailbox

-- | Get the value of the @Reply-To:@ field.
replyTo :: Headers -> Maybe [Recipient]
replyTo = structuredField "Reply-To" P.recipientList

-- | Get the value of the @To:@ field.
to :: Headers -> Maybe [Recipient]
to = structuredField "To" P.recipientList

-- | Get the value of the @Cc:@ field.
cc :: Headers -> Maybe [Recipient]
cc = structuredField "Cc" P.recipientList

-- | Get the value of the @Bcc:@ field.
bcc :: Headers -> Maybe (Maybe [Recipient])
bcc = structuredField "Bcc" (optional P.recipientList)

-- | Get the value of the @Message-ID:@ field.
messageID :: Headers -> Maybe MessageID
messageID = structuredField "Message-ID" P.messageID

-- | Get the value of the @In-Reply-To:@ field.
inReplyTo :: Headers -> Maybe [MessageID]
inReplyTo = structuredField "In-Reply-To" (many1 P.messageID)

-- | Get the value of the @References:@ field.
references :: Headers -> Maybe [MessageID]
references = structuredField "References" (many1 P.messageID)

-- | Get the value of the @Subject:@ field.
subject :: Headers -> Maybe L.Text
subject = field "Subject" P.unstructured

-- | Get the value of the @Comments:@ field.
comments :: Headers -> Maybe L.Text
comments = field "Comments" P.unstructured

-- | Get the value of the @Keywords:@ field.
keywords :: Headers -> Maybe [L.Text]
keywords = structuredField "Keywords" P.phraseList

-- | Get the value of the @Resent-Date:@ field.
resentDate :: Headers -> Maybe ZonedTime
resentDate = structuredField "Resent-Date" P.dateTime

-- | Get the value of the @Resent-From:@ field.
resentFrom :: Headers -> Maybe [Mailbox]
resentFrom = structuredField "Resent-From" P.mailboxList

-- | Get the value of the @Resent-Sender:@ field.
resentSender :: Headers -> Maybe Mailbox
resentSender = structuredField "Resent-Sender" P.mailbox

-- | Get the value of the @Resent-To:@ field.
resentTo :: Headers -> Maybe [Recipient]
resentTo = structuredField "Resent-To" P.recipientList

-- | Get the value of the @Resent-Cc:@ field.
resentCc :: Headers -> Maybe [Recipient]
resentCc = structuredField "Resent-Cc" P.recipientList

-- | Get the value of the @Resent-Bcc:@ field.
resentBcc :: Headers -> Maybe (Maybe [Recipient])
resentBcc = structuredField "Resent-Bcc" (optional P.recipientList)

-- | Get the value of the @Resent-Message-ID:@ field.
resentMessageID :: Headers -> Maybe MessageID
resentMessageID = structuredField "Resent-Message-ID" P.messageID

-- | Get the value of the @MIME-Version:@ field.
mimeVersion :: Headers -> Maybe (Int, Int)
mimeVersion = structuredField "MIME-Version" P.mimeVersion

-- | Get the value of the @Content-Type:@ field.
contentType :: Headers -> Maybe (MimeType, Parameters)
contentType = structuredField "Content-Type" P.contentType

-- | Get the value of the @Content-Transfer-Encoding:@ field.
contentTransferEncoding :: Headers -> Maybe (CI B.ByteString)
contentTransferEncoding =
    structuredField "Content-Transfer-Encoding" P.contentTransferEncoding

-- | Get the value of the @Content-ID:@ field.
contentID :: Headers -> Maybe MessageID
contentID = structuredField "Content-ID" P.messageID
