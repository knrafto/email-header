{-# LANGUAGE OverloadedStrings #-}
-- | Rendering arbitrary values to arbitrary 'L.ByteString's.
module DocGen
    ( -- * 'DocGen' type
      DocGen(..)
    , renderDoc
      -- * Manipulation
    , literal
    , option
    , oneof
    , frequency
    , listOf
    ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy         as L
import           Data.ByteString.Lazy.Builder
import           Data.Monoid
import           Data.String
import           Test.QuickCheck.Gen          (Gen)
import qualified Test.QuickCheck.Gen          as Gen

-- | A randomized generator for 'L.ByteString's.
newtype DocGen = DocGen { runDoc :: Gen Builder }

instance Monoid DocGen where
    mempty = DocGen $ pure mempty
    DocGen a `mappend` DocGen b = DocGen $ liftA2 mappend a b

instance IsString DocGen where
    fromString = literal . string8

-- | Render a document to a lazy 'L.ByteString'.
renderDoc :: DocGen -> Gen L.ByteString
renderDoc = fmap toLazyByteString . runDoc

-- | A deterministic literal value.
literal :: Builder -> DocGen
literal = DocGen . pure

-- | @'option' d@ renders either @d@ or nothing.
option :: DocGen -> DocGen
option d = oneof [mempty, d]

-- | Select a random item to render.
oneof :: [DocGen] -> DocGen
oneof = DocGen . Gen.oneof . map runDoc

-- | Select a weighted item to render.
frequency :: [(Int, DocGen)] -> DocGen
frequency = DocGen . Gen.frequency . map toGen
  where
    toGen (i, d) = (i, runDoc d)

-- | Repeat a document a random number of times (possibly zero).
listOf :: DocGen -> DocGen
listOf = DocGen . fmap mconcat . Gen.listOf . runDoc
