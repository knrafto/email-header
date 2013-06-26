{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of common header values.
module Network.Email.Parse
    ( phrase
    , phraseList
    , unstructured
    , dateTime
    , address
    , mailbox
    , mailboxList
    , recipient
    , recipientList
    , messageId
    ) where

import           Control.Applicative
import           Data.Attoparsec              (Parser)
import           Data.Attoparsec.Combinator
import qualified Data.ByteString              as B
import           Data.Char
import           Data.List
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy               as L
import           Data.Text.Lazy.Builder

import           Network.Email.Parse.Internal
import           Network.Email.Types

-- | Combine a list of text elements (atoms, quoted strings, encoded words,
-- etc.) into a larger phrase.
fromElements :: [T.Text] -> L.Text
fromElements =
    toLazyText . mconcat . intersperse (singleton ' ') .
    map fromText . concatMap splitElement
  where
    splitElement = filter (not . T.null) . T.split isSep
    isSep c      = isControl c || isSpace c

-- | Parse a phrase. Adjacent encoded words are concatenated. White space
-- is reduced to a single space, except when quoted or part of an encoded
-- word.
phrase :: Parser L.Text
phrase = fromElements <$> many1 element
  where
    element = T.concat     <$> many1 encodedWord
          <|> decodeLatin1 <$> quotedString
          <|> decodeLatin1 <$> dotAtom

-- | Parse a comma-separated list of phrases.
phraseList :: Parser [L.Text]
phraseList = commaSep phrase

-- | Parse unstructured text. Adjacent encoded words are concatenated.
-- White space is reduced to a single space.
unstructured :: Parser L.Text
unstructured = fromElements <$> many element
  where
    element = T.concat     <$> many1 encodedWord
          <|> decodeLatin1 <$> textToken

-- | Parse an email address.
address :: Parser Address
address = Address <$> addrSpec

-- | Parse an address specification in angle brackets.
angleAddrSpec :: Parser B.ByteString
angleAddrSpec = character leftAngle *> addrSpec <* character rightAngle
  where
    leftAngle  = 60
    rightAngle = 62

-- | Parse an email address in angle brackets.
angleAddr :: Parser Address
angleAddr = Address <$> angleAddrSpec

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

-- | Parse a message identifier,
messageId :: Parser MessageId
messageId = MessageId <$> angleAddrSpec
