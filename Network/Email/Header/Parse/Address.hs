-- | Parsing of email addresses and mailboxes.
module Network.Email.Header.Parse.Address
    ( address
    , mailbox
    , mailboxList
    , recipient
    , recipientList
    ) where

import           Control.Applicative
import           Data.Attoparsec       (Parser)
import qualified Data.Attoparsec.Char8 as A8

import Network.Email.Header.Parse.Internal
import Network.Email.Header.Parse.Text
import Network.Email.Types

-- | Parse an email address in angle brackets.
angleAddr :: Parser Address
angleAddr = Address <$> angleAddrSpec

-- | Parse an email address.
address :: Parser Address
address = Address <$> addrSpec

-- | Parse a 'Mailbox'.
mailbox :: Parser Mailbox
mailbox = Mailbox <$> optional phrase <*> angleAddr
      <|> Mailbox Nothing <$> address

-- | Parse a list of @'Mailbox'es@.
mailboxList :: Parser [Mailbox]
mailboxList = commaSep mailbox

-- | Parse a 'Recipient'.
recipient :: Parser Recipient
recipient = Group <$> phrase <* A8.char ':'
                  <*> mailboxList <* A8.char ';'
        <|> Individual <$> mailbox

-- | Parse a list of @'Recipient's@.
recipientList :: Parser [Recipient]
recipientList = commaSep recipient
