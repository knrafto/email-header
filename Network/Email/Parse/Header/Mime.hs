-- | Parsing of MIME header fields.
module Network.Email.Parse.Header.Mime
    ( mimeVersion
    , contentType
    ) where

import           Control.Applicative
import           Data.Attoparsec       (Parser)
import qualified Data.Attoparsec.Char8 as A8
import qualified Data.Map              as Map

import Network.Email.Parse.Header.Internal
import Network.Email.Types

-- | Parse the MIME version (which should be 1.0).
mimeVersion :: Parser (Int, Int)
mimeVersion = (,) <$> digits 1 <* padded (A8.char '.') <*> digits 1

-- | Parse the content type.
contentType :: Parser (MimeType, Parameters)
contentType = liftA2 (,)
    (MimeType <$> token <* padded (A8.char '/') <*> token <* cfws)
    (Map.fromList <$> many (A8.char ';' *> padded parameter))
  where
    parameter = (,) <$> token <* padded (A8.char '=')
                    <*> (token <|> quotedString)
