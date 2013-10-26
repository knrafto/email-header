-- | Parsing of MIME header fields.
module Network.Email.Parse.Header.Mime
    ( mimeVersion
    , contentType
    ) where

import           Control.Applicative
import           Data.Attoparsec     (Parser)
import qualified Data.Map            as Map

import           Network.Email.Parse.Header.Internal
import           Network.Email.Types

-- | Parse a pair.
pair :: Applicative f => f a -> f b -> f (a, b)
pair = liftA2 (,)

-- | Parse the MIME version (which should be 1.0).
mimeVersion :: Parser (Int, Int)
mimeVersion = pair digit (character dot *> digit)
  where
    dot   = 46
    digit = lexeme (digits 1)

-- | Parse the content type.
contentType :: Parser (MimeType, Parameters)
contentType = pair
    (MimeType <$> token <*> (character slash *> token))
    (Map.fromList <$> many (character semicolon *> parameter))
  where
    slash     = 47
    semicolon = 59
    equals    = 61

    parameter = pair token (character equals *> (token <|> quotedString))
