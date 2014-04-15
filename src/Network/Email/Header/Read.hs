{-# LANGUAGE OverloadedStrings #-}
-- | Reading common header fields.
-- This module is intended to be imported qualified:
--
-- > import qualified Network.Email.Header.Read as H
module Network.Email.Header.Read
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
import           Control.Monad.Catch
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Lazy
import qualified Data.ByteString             as B
import           Data.CaseInsensitive        (CI)
import qualified Data.Text.Lazy              as L
import           Data.Time.LocalTime

import qualified Network.Email.Header.Parser as P
import           Network.Email.Header.Types

-- | Lookup and parse a header with a parser.
field :: MonadThrow m => HeaderName -> Parser a -> Headers -> m a
field k p hs = do
    body <- case lookup k hs of
        Nothing -> throwM $ MissingHeader k
        Just b  -> return b
    case parse p body of
        Fail _ _ s -> throwM $ HeaderParseError (k, body) s
        Done _ a   -> return a

-- | Lookup and parse a structured header with a parser. This skips initial
-- comments and folding white space, and ensures that the entire body is
-- consumed by the parser.
structuredField :: MonadThrow m => HeaderName -> Parser a -> Headers -> m a
structuredField k p = field k (P.cfws *> p <* endOfInput)

-- | Get the value of the @Date:@ field.
date :: MonadThrow m => Headers -> m ZonedTime
date = structuredField "Date" P.dateTime

-- | Get the value of the @From:@ field.
from :: MonadThrow m => Headers -> m [Mailbox]
from = structuredField "From" P.mailboxList

-- | Get the value of the @Sender:@ field.
sender :: MonadThrow m => Headers -> m Mailbox
sender = structuredField "Sender" P.mailbox

-- | Get the value of the @Reply-To:@ field.
replyTo :: MonadThrow m => Headers -> m [Recipient]
replyTo = structuredField "Reply-To" P.recipientList

-- | Get the value of the @To:@ field.
to :: MonadThrow m => Headers -> m [Recipient]
to = structuredField "To" P.recipientList

-- | Get the value of the @Cc:@ field.
cc :: MonadThrow m => Headers -> m [Recipient]
cc = structuredField "Cc" P.recipientList

-- | Get the value of the @Bcc:@ field.
bcc :: MonadThrow m => Headers -> m (Maybe [Recipient])
bcc = structuredField "Bcc" (optional P.recipientList)

-- | Get the value of the @Message-ID:@ field.
messageID :: MonadThrow m => Headers -> m MessageID
messageID = structuredField "Message-ID" P.messageID

-- | Get the value of the @In-Reply-To:@ field.
inReplyTo :: MonadThrow m => Headers -> m [MessageID]
inReplyTo = structuredField "In-Reply-To" (many1 P.messageID)

-- | Get the value of the @References:@ field.
references :: MonadThrow m => Headers -> m [MessageID]
references = structuredField "References" (many1 P.messageID)

-- | Get the value of the @Subject:@ field.
subject :: MonadThrow m => Headers -> m L.Text
subject = field "Subject" P.unstructured

-- | Get the value of the @Comments:@ field.
comments :: MonadThrow m => Headers -> m L.Text
comments = field "Comments" P.unstructured

-- | Get the value of the @Keywords:@ field.
keywords :: MonadThrow m => Headers -> m [L.Text]
keywords = structuredField "Keywords" P.phraseList

-- | Get the value of the @Resent-Date:@ field.
resentDate :: MonadThrow m => Headers -> m ZonedTime
resentDate = structuredField "Resent-Date" P.dateTime

-- | Get the value of the @Resent-From:@ field.
resentFrom :: MonadThrow m => Headers -> m [Mailbox]
resentFrom = structuredField "Resent-From" P.mailboxList

-- | Get the value of the @Resent-Sender:@ field.
resentSender :: MonadThrow m => Headers -> m Mailbox
resentSender = structuredField "Resent-Sender" P.mailbox

-- | Get the value of the @Resent-To:@ field.
resentTo :: MonadThrow m => Headers -> m [Recipient]
resentTo = structuredField "Resent-To" P.recipientList

-- | Get the value of the @Resent-Cc:@ field.
resentCc :: MonadThrow m => Headers -> m [Recipient]
resentCc = structuredField "Resent-Cc" P.recipientList

-- | Get the value of the @Resent-Bcc:@ field.
resentBcc :: MonadThrow m => Headers -> m (Maybe [Recipient])
resentBcc = structuredField "Resent-Bcc" (optional P.recipientList)

-- | Get the value of the @Resent-Message-ID:@ field.
resentMessageID :: MonadThrow m => Headers -> m MessageID
resentMessageID = structuredField "Resent-Message-ID" P.messageID

-- | Get the value of the @MIME-Version:@ field.
mimeVersion :: MonadThrow m => Headers -> m (Int, Int)
mimeVersion = structuredField "MIME-Version" P.mimeVersion

-- | Get the value of the @Content-Type:@ field.
contentType :: MonadThrow m => Headers -> m (MimeType, Parameters)
contentType = structuredField "Content-Type" P.contentType

-- | Get the value of the @Content-Transfer-Encoding:@ field.
contentTransferEncoding :: MonadThrow m => Headers -> m (CI B.ByteString)
contentTransferEncoding =
    structuredField "Content-Transfer-Encoding" P.contentTransferEncoding

-- | Get the value of the @Content-ID:@ field.
contentID :: MonadThrow m => Headers -> m MessageID
contentID = structuredField "Content-ID" P.messageID
