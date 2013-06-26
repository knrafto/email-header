{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of common header values.
module Network.Email.Parse
    ( phrase
    , phraseList
    , unstructured
    , dateTime
    , addrSpec
    ) where

import           Control.Applicative
import           Data.Attoparsec              (Parser)
import           Data.Attoparsec.Combinator
import           Data.Char
import           Data.List
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy               as L
import           Data.Text.Lazy.Builder

import           Network.Email.Parse.Internal

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
