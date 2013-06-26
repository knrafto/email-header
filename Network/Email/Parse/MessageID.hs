-- | Parsing message identifiers.
module Network.Email.Parse.MessageID
    ( messageID
    , messageIDList
    ) where

import Data.Attoparsec              (Parser)
import Data.Attoparsec.Combinator
import Data.Functor

import Network.Email.Parse.Internal
import Network.Email.Types

-- | Parse a message identifier.
messageID :: Parser MessageID
messageID = MessageID <$> angleAddrSpec

-- | Parse a list of message identifiers.
messageIDList :: Parser [MessageID]
messageIDList = many1 messageID
