-- | Email header types, renamed slightly from RFC 8322.
module Network.Email.Header.Types
    ( Headers
    , HeaderName
    , HeaderField
    , Address(..)
    , Mailbox(..)
    , Recipient(..)
    , MessageID(..)
    , MimeType(..)
    , Parameters
    ) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.CaseInsensitive (CI)
import           Data.Map.Strict      (Map)
import qualified Data.Text.Lazy       as L

-- | A set of email headers.
type Headers = [(HeaderName, HeaderField)]

-- | An email header name.
type HeaderName = CI B.ByteString

-- | The email header field.
type HeaderField = L.ByteString

-- | An email address.
newtype Address = Address B.ByteString
    deriving (Eq, Ord, Show)

-- | A 'Mailbox' receives mail.
data Mailbox = Mailbox
    { displayName    :: Maybe L.Text
    , mailboxAddress :: Address
    } deriving (Eq, Show)

-- | A 'Recipient' is used to indicate senders and recipients of messages.
-- It may either be an individual 'Mailbox', or a named group of
-- @'Mailbox'es@.
data Recipient
    = Individual Mailbox
    | Group L.Text [Mailbox]
    deriving (Eq, Show)

-- | A message identifier.
newtype MessageID = MessageID B.ByteString
    deriving (Eq, Ord, Show)

-- | A MIME type.
data MimeType = MimeType
    { mimeType    :: CI B.ByteString
    , mimeSubtype :: CI B.ByteString
    } deriving (Eq, Ord, Show)

-- | Content type parameters.
type Parameters = Map (CI B.ByteString) B.ByteString
