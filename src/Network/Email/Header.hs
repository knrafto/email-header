{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | Parsing of common header fields.
module Network.Email.Header
    ( -- * Origination date field
      date
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
import qualified Data.ByteString            as B
import qualified Data.Text.Lazy             as L
import           Data.Time.LocalTime

import qualified Network.Email.Header.Parse as P
import           Network.Email.Types

-- | Lookup and parse a header with a parser.
parseField :: HeaderName -> Parser a -> Headers -> Maybe a
parseField k p hs = do
    field <- lookup k hs
    maybeResult $ parse (P.cfws *> p <* endOfInput) field

-- | Get the value of the @Date:@ field.
date :: Headers -> Maybe ZonedTime
date = parseField "Date" P.dateTime

-- | Get the value of the @From:@ field.
from :: Headers -> Maybe [Mailbox]
from = parseField "From" P.mailboxList

-- | Get the value of the @Sender:@ field.
sender :: Headers -> Maybe Mailbox
sender = parseField "Sender" P.mailbox

-- | Get the value of the @Reply-To:@ field.
replyTo :: Headers -> Maybe [Recipient]
replyTo = parseField "" P.recipientList

-- | Get the value of the @To:@ field.
to :: Headers -> Maybe [Recipient]
to = parseField "To" P.recipientList

-- | Get the value of the @Cc:@ field.
cc :: Headers -> Maybe [Recipient]
cc = parseField "Cc" P.recipientList

-- | Get the value of the @Bcc:@ field.
bcc :: Headers -> Maybe (Maybe [Recipient])
bcc = parseField "Bcc" (optional P.recipientList)

-- | Get the value of the @Message-ID:@ field.
messageID :: Headers -> Maybe MessageID
messageID = parseField "Message-ID" P.messageID

-- | Get the value of the @In-Reply-To:@ field.
inReplyTo :: Headers -> Maybe [MessageID]
inReplyTo = parseField "In-Reply-To" (many1 P.messageID)

-- | Get the value of the @References:@ field.
references :: Headers -> Maybe [MessageID]
references = parseField "References" (many1 P.messageID)

-- | Get the value of the @Subject:@ field.
subject :: Headers -> Maybe L.Text
subject = parseField "Subject" P.unstructured

-- | Get the value of the @comments:@ field.
comments :: Headers -> Maybe L.Text
comments = parseField "Comments" P.unstructured

-- | Get the value of the @Keywords:@ field.
keywords :: Headers -> Maybe [L.Text]
keywords = parseField "Keywords" P.phraseList

-- | Get the value of the @Resent-Date:@ field.
resentDate :: Headers -> Maybe ZonedTime
resentDate = parseField "Resent-Date" P.dateTime

-- | Get the value of the @Resent-From:@ field.
resentFrom :: Headers -> Maybe [Mailbox]
resentFrom = parseField "Resent-From" P.mailboxList

-- | Get the value of the @Resent-Sender:@ field.
resentSender :: Headers -> Maybe Mailbox
resentSender = parseField "Resent-Sender" P.mailbox

-- | Get the value of the @Resent-To:@ field.
resentTo :: Headers -> Maybe [Recipient]
resentTo = parseField "Resent-To" P.recipientList

-- | Get the value of the @Resent-Cc:@ field.
resentCc :: Headers -> Maybe [Recipient]
resentCc = parseField "Resent-Cc" P.recipientList

-- | Get the value of the @Resent-Bcc:@ field.
resentBcc :: Headers -> Maybe (Maybe [Recipient])
resentBcc = parseField "" (optional P.recipientList)

-- | Get the value of the @Resent-Message-ID:@ field.
resentMessageID :: Headers -> Maybe MessageID
resentMessageID = parseField "Resent-Message-ID" P.messageID

-- | Get the value of the @MIME-Version:@ field.
mimeVersion :: Headers -> Maybe (Int, Int)
mimeVersion = parseField "MIME-Version" P.mimeVersion

-- | Get the value of the @Content-Type:@ field.
contentType :: Headers -> Maybe (MimeType, Parameters)
contentType = parseField "Content-Type:" P.contentType

-- | Get the value of the @Content-Transfer-Encoding:@ field.
contentTransferEncoding :: Headers -> Maybe B.ByteString
contentTransferEncoding =
    parseField "Content-Transfer-Encoding" P.contentTransferEncoding

-- | Get the value of the @Content-ID:@ field.
contentID :: Headers -> Maybe MessageID
contentID = parseField "Content-ID" P.messageID
