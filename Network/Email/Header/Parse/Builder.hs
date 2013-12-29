-- | Parsing with builders.
module Network.Email.Header.Parse.Builder
    ( -- * Combinators
      (<+>)
    , optional
    , many
    , many1
    , sepBy
    , sepBy1
      -- * Builder parsers
    , anyWord8
    , word8
    , char8
    , satisfy
    ) where

import           Control.Applicative          hiding (many, optional)
import qualified Control.Applicative          as Applicative
import           Data.Attoparsec              (Parser)
import qualified Data.Attoparsec              as A
import qualified Data.Attoparsec.Char8        as A8
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as B
import           Data.Monoid
import           Data.Word

infixl 3 <+>

-- | Concatenate two results.
(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

-- | Zero or one.
optional :: (Alternative f, Monoid a) => f a -> f a
optional p = p <|> pure mempty

-- | Zero or more.
many :: (Alternative f, Monoid a) => f a -> f a
many p = mconcat <$> Applicative.many p

-- | One or more.
many1 :: (Alternative f, Monoid a) => f a -> f a
many1 p = mconcat <$> Applicative.some p

-- | @sepBy p sep@ applies @p@ zero or more times, separated by @sep@. The
-- separators are preserved in the output.
sepBy :: (Alternative f, Monoid a) => f a -> f a -> f a
sepBy p sep = optional (sepBy1 p sep)

-- | @sepBy p sep@ applies @p@ one or more times, separated by @sep@. The
-- separators are preserved in the output.
sepBy1 :: (Alternative f, Monoid a) => f a -> f a -> f a
sepBy1 p sep = go
  where
    go = p <+> optional (sep <+> optional go)

-- | Parse any 'Word8' value'.
anyWord8 :: Parser Builder
anyWord8 = B.word8 <$> A.anyWord8

-- | Parse a specific 'Word8'.
word8 :: Word8 -> Parser Builder
word8 w = B.word8 w <$ A.word8 w

-- | Parse a specific character.
char8 :: Char -> Parser Builder
char8 c = B.char8 c <$ A8.char8 c

-- | Parse a character that satisfies a predicate.
satisfy :: (Char -> Bool) -> Parser Builder
satisfy p = B.char8 <$> A8.satisfy p
