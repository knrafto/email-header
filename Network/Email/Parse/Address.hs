-- | Parsing of email addresses and mailboxes.
module Network.Email.Parse.Address
    ( address
    , mailbox
    , mailboxList
    , recipient
    , recipientList
    ) where

import Control.Applicative
import Data.Attoparsec              (Parser)

import Network.Email.Parse.Internal
import Network.Email.Parse.Text
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
recipient = Group <$> phrase <* character colon
                  <*> mailboxList <* character semicolon
        <|> Individual <$> mailbox
  where
    colon     = 58
    semicolon = 59

-- | Parse a list of @'Recipient's@.
recipientList :: Parser [Recipient]
recipientList = commaSep recipient
