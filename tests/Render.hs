{-# LANGUAGE OverloadedStrings #-}
module Render
    ( Render(..)
    , Doc(..)
    , renderDoc
    , literal
    , option
    , oneof
    , frequency
    , many
    ) where

import           Control.Applicative
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import           Data.ByteString.Lazy.Builder
import           Data.Monoid
import           Data.String
import           Test.QuickCheck.Gen          (Gen)
import qualified Test.QuickCheck.Gen          as Gen

class Render a where
    render :: a -> Doc

newtype Doc = Doc { runDoc :: Gen Builder }

instance Monoid Doc where
    mempty = Doc $ pure mempty
    Doc a `mappend` Doc b = Doc $ liftA2 mappend a b

instance IsString Doc where
    fromString = literal . string8

renderDoc :: Doc -> Gen B.ByteString
renderDoc = fmap (L.toStrict . toLazyByteString) . runDoc

literal :: Builder -> Doc
literal = Doc . pure

option :: Doc -> Doc
option d = oneof [mempty, d]

oneof :: [Doc] -> Doc
oneof = Doc . Gen.oneof . map runDoc

frequency :: [(Int, Doc)] -> Doc
frequency = Doc . Gen.frequency . map toGen
  where
    toGen (i, d) = (i, runDoc d)

listOf :: Doc -> Doc
listOf = Doc . fmap mconcat . Gen.listOf . runDoc
