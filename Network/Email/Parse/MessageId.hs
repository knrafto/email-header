-- | Parsing message identifiers.
module Network.Email.Parse.MessageId
    ( messageId
    , messageIdList
    ) where

import Data.Attoparsec              (Parser)
import Data.Attoparsec.Combinator
import Data.Functor

import Network.Email.Parse.Internal
import Network.Email.Types

-- | Parse a message identifier.
messageId :: Parser MessageId
messageId = MessageId <$> angleAddrSpec

-- | Parse a list of message identifiers.
messageIdList :: Parser [MessageId]
messageIdList = many1 messageId
