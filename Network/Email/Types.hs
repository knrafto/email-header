-- | Email types, renamed slightly from RFC 8322.
module Network.Email.Types
    ( Address(..)
    , Mailbox(..)
    , Recipient(..)
    ) where

import qualified Data.ByteString as B
import qualified Data.Text.Lazy  as L

-- | An email address.
newtype Address = Address { showAddress :: B.ByteString }
    deriving (Eq, Ord, Show)

-- | A 'Mailbox' receives mail.
data Mailbox = Mailbox
    { displayName    :: Maybe L.Text
    , mailboxAddress :: Address
    } deriving (Show)

-- | A 'Recipient' is used to indicate senders and recipients of messages.
-- It may either be an individual 'Mailbox', or a named group of
-- @'Mailbox'es@.
data Recipient
    = Individual Mailbox
    | Group L.Text [Mailbox]
    deriving (Show)
