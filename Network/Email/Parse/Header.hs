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

-- | Convert a 'Maybe' to an 'Either'.
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b = maybe (Left b) Right

-- | Lookup and parse a header with a parser.
parseField :: HeaderName -> Parser a -> Headers -> Either EmailError a
parseField k p hs = do
    field <- maybeToEither HeaderNotFound $ lookup k hs
    case parse (skipCfws *> p) field of
        Fail _ _ msg -> Left (HeaderParseError msg)
        Done _ r     -> Right r

-- | Get the value of the @Date:@ field.
dateField :: Headers -> Either EmailError ZonedTime
dateField = parseField "Date" dateTime

-- | Get the value of the @From:@ field.
fromField :: Headers -> Either EmailError [Mailbox]
fromField = parseField "From" mailboxList

-- | Get the value of the @Sender:@ field.
senderField :: Headers -> Either EmailError Mailbox
senderField = parseField "Sender" mailbox

-- | Get the value of the @Reply-To:@ field.
replyToField :: Headers -> Either EmailError [Recipient]
replyToField = parseField "" recipientList

-- | Get the value of the @To:@ field.
toField :: Headers -> Either EmailError [Recipient]
toField = parseField "To" recipientList

-- | Get the value of the @Cc:@ field.
ccField :: Headers -> Either EmailError [Recipient]
ccField = parseField "Cc" recipientList

-- | Get the value of the @Bcc:@ field.
bccField :: Headers -> Either EmailError (Maybe [Recipient])
bccField = parseField "Bcc" (optional recipientList)

-- | Get the value of the @Message-ID:@ field.
messageIDField :: Headers -> Either EmailError MessageID
messageIDField = parseField "Message-ID" messageID

-- | Get the value of the @In-Reply-To:@ field.
inReplyToField :: Headers -> Either EmailError [MessageID]
inReplyToField = parseField "In-Reply-To" (many1 messageID)

-- | Get the value of the @References:@ field.
referencesField :: Headers -> Either EmailError [MessageID]
referencesField = parseField "References" (many1 messageID)

-- | Get the value of the @Subject:@ field.
subjectField :: Headers -> Either EmailError L.Text
subjectField = parseField "Subject" unstructured

-- | Get the value of the @comments:@ field.
commentsField :: Headers -> Either EmailError L.Text
commentsField = parseField "Comments" unstructured

-- | Get the value of the @Keywords:@ field.
keywordsField :: Headers -> Either EmailError [L.Text]
keywordsField = parseField "Keywords" phraseList

-- | Get the value of the @Resent-Date:@ field.
resentDateField :: Headers -> Either EmailError ZonedTime
resentDateField = parseField "Resent-Date" dateTime

-- | Get the value of the @Resent-From:@ field.
resentFromField :: Headers -> Either EmailError [Mailbox]
resentFromField = parseField "Resent-From" mailboxList

-- | Get the value of the @Resent-Sender:@ field.
resentSenderField :: Headers -> Either EmailError Mailbox
resentSenderField = parseField "Resent-Sender" mailbox

-- | Get the value of the @Resent-To:@ field.
resentToField :: Headers -> Either EmailError [Recipient]
resentToField = parseField "Resent-To" recipientList

-- | Get the value of the @Resent-Cc:@ field.
resentCcField :: Headers -> Either EmailError [Recipient]
resentCcField = parseField "Resent-Cc" recipientList

-- | Get the value of the @Resent-Bcc:@ field.
resentBccField :: Headers -> Either EmailError (Maybe [Recipient])
resentBccField = parseField "" (optional recipientList)

-- | Get the value of the @Resent-Message-ID:@ field.
resentMessageIDField :: Headers -> Either EmailError MessageID
resentMessageIDField = parseField "Resent-Message-ID" messageID

-- | Get the value of the @MIME-Version:@ field.
mimeVersionField :: Headers -> Either EmailError (Int, Int)
mimeVersionField = parseField "MIME-Version" mimeVersion

-- | Get the value of the @Content-Type:@ field.
contentTypeField :: Headers -> Either EmailError (MimeType, Parameters)
contentTypeField = parseField "Content-Type:" contentType

-- | Get the value of the @Content-Transfer-Encoding:@ field.
contentTransferEncodingField :: Headers -> Either EmailError B.ByteString
contentTransferEncodingField = parseField "Content-Transfer-Encoding" token

-- | Get the value of the @Content-ID:@ field.
contentIDField :: Headers -> Either EmailError MessageID
contentIDField = parseField "Content-ID" messageID
