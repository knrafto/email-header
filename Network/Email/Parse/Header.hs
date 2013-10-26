{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | Parsing of common header fields.
module Network.Email.Parse.Header
    ( -- * Origination date field
      dateField
      -- * Originator fields
    , fromField
    , senderField
    , replyToField
      -- * Destination address fields
    , toField
    , ccField
    , bccField
      -- * Identification fields
    , messageIDField
    , inReplyToField
    , referencesField
      -- * Informational fields
    , subjectField
    , commentsField
    , keywordsField
      -- * Resent fields
    , resentDateField
    , resentFromField
    , resentSenderField
    , resentToField
    , resentCcField
    , resentBccField
    , resentMessageIDField
      -- * MIME fields
    , mimeVersionField
    , contentTypeField
    , contentTransferEncodingField
    , contentIDField
    ) where

import           Control.Applicative
import           Control.Monad.Error
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Lazy
import qualified Data.ByteString            as B
import qualified Data.Text.Lazy             as L
import           Data.Time.LocalTime

import           Network.Email.Parse.Header.Address
import           Network.Email.Parse.Header.DateTime
import           Network.Email.Parse.Header.Internal
import           Network.Email.Parse.Header.MessageID
import           Network.Email.Parse.Header.Mime
import           Network.Email.Parse.Header.Text
import           Network.Email.Types

-- | Lookup and parse a header with a parser.
parseField
    :: MonadError EmailError m
    => HeaderName
    -> Parser a
    -> Headers
    -> m a
parseField k p hs = do
    field <- maybe (throwError HeaderNotFound) return $ lookup k hs
    case parse (skipCfws *> p) field of
        Fail _ _ msg -> throwError (HeaderParseError msg)
        Done _ r     -> return r

-- | Get the value of the @Date:@ field.
dateField :: MonadError EmailError m => Headers -> m ZonedTime
dateField = parseField "Date" dateTime

-- | Get the value of the @From:@ field.
fromField :: MonadError EmailError m => Headers -> m [Mailbox]
fromField = parseField "From" mailboxList

-- | Get the value of the @Sender:@ field.
senderField :: MonadError EmailError m => Headers -> m Mailbox
senderField = parseField "Sender" mailbox

-- | Get the value of the @Reply-To:@ field.
replyToField :: MonadError EmailError m => Headers -> m [Recipient]
replyToField = parseField "" recipientList

-- | Get the value of the @To:@ field.
toField :: MonadError EmailError m => Headers -> m [Recipient]
toField = parseField "To" recipientList

-- | Get the value of the @Cc:@ field.
ccField :: MonadError EmailError m => Headers -> m [Recipient]
ccField = parseField "Cc" recipientList

-- | Get the value of the @Bcc:@ field.
bccField :: MonadError EmailError m => Headers -> m (Maybe [Recipient])
bccField = parseField "Bcc" (optional recipientList)

-- | Get the value of the @Message-ID:@ field.
messageIDField :: MonadError EmailError m => Headers -> m MessageID
messageIDField = parseField "Message-ID" messageID

-- | Get the value of the @In-Reply-To:@ field.
inReplyToField :: MonadError EmailError m => Headers -> m [MessageID]
inReplyToField = parseField "In-Reply-To" (many1 messageID)

-- | Get the value of the @References:@ field.
referencesField :: MonadError EmailError m => Headers -> m [MessageID]
referencesField = parseField "References" (many1 messageID)

-- | Get the value of the @Subject:@ field.
subjectField :: MonadError EmailError m => Headers -> m L.Text
subjectField = parseField "Subject" unstructured

-- | Get the value of the @comments:@ field.
commentsField :: MonadError EmailError m => Headers -> m L.Text
commentsField = parseField "Comments" unstructured

-- | Get the value of the @Keywords:@ field.
keywordsField :: MonadError EmailError m => Headers -> m [L.Text]
keywordsField = parseField "Keywords" phraseList

-- | Get the value of the @Resent-Date:@ field.
resentDateField :: MonadError EmailError m => Headers -> m ZonedTime
resentDateField = parseField "Resent-Date" dateTime

-- | Get the value of the @Resent-From:@ field.
resentFromField :: MonadError EmailError m => Headers -> m [Mailbox]
resentFromField = parseField "Resent-From" mailboxList

-- | Get the value of the @Resent-Sender:@ field.
resentSenderField :: MonadError EmailError m => Headers -> m Mailbox
resentSenderField = parseField "Resent-Sender" mailbox

-- | Get the value of the @Resent-To:@ field.
resentToField :: MonadError EmailError m => Headers -> m [Recipient]
resentToField = parseField "Resent-To" recipientList

-- | Get the value of the @Resent-Cc:@ field.
resentCcField :: MonadError EmailError m => Headers -> m [Recipient]
resentCcField = parseField "Resent-Cc" recipientList

-- | Get the value of the @Resent-Bcc:@ field.
resentBccField :: MonadError EmailError m => Headers -> m (Maybe [Recipient])
resentBccField = parseField "" (optional recipientList)

-- | Get the value of the @Resent-Message-ID:@ field.
resentMessageIDField :: MonadError EmailError m => Headers -> m MessageID
resentMessageIDField = parseField "Resent-Message-ID" messageID

-- | Get the value of the @MIME-Version:@ field.
mimeVersionField :: MonadError EmailError m => Headers -> m (Int, Int)
mimeVersionField = parseField "MIME-Version" mimeVersion

-- | Get the value of the @Content-Type:@ field.
contentTypeField
    :: MonadError EmailError m => Headers -> m (MimeType, Parameters)
contentTypeField = parseField "Content-Type:" contentType

-- | Get the value of the @Content-Transfer-Encoding:@ field.
contentTransferEncodingField
    :: MonadError EmailError m => Headers -> m B.ByteString
contentTransferEncodingField = parseField "Content-Transfer-Encoding" token

-- | Get the value of the @Content-ID:@ field.
contentIDField :: MonadError EmailError m => Headers -> m MessageID
contentIDField = parseField "Content-ID" messageID
