-- | Charset conversions.
module Network.Email.Charset
    ( -- * Charsets
      Charset
    , charsetName
      -- * Lookup
    , charsets
    , lookupCharset
    , defaultCharset
      -- * Conversion
    , fromUnicode
    , toUnicode
    ) where

import           Prelude               hiding (lookup)

import           Data.ByteString       (ByteString)
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text.ICU.Convert as ICU
import           System.IO.Unsafe

-- | A charset.
newtype Charset = Charset String
    deriving (Show)

instance Eq Charset where
    a == b = compare a b == EQ

instance Ord Charset where
    compare (Charset a) (Charset b) = ICU.compareNames a b

-- | The name of a charset.
charsetName :: Charset -> String
charsetName (Charset s) = s

-- | All charset names and aliases.
charsets :: Set Charset
charsets = Set.fromList . map Charset . filter legalName $
    concatMap ICU.aliases ICU.converterNames
  where
    legalName = all (`notElem` ",.=?")

-- | Lookup a charset from a name or alias.
lookupCharset :: String -> Maybe Charset
lookupCharset name = case Set.lookupLE c charsets of
    Just c' | c' == c -> Just c'
    _                 -> Nothing
  where
    c = Charset name

-- | The default charset, UTF-8.
defaultCharset :: Charset
defaultCharset = Charset "UTF-8"

-- | Unsafely load a converter from a charset. The resulting converter is not
-- thread-safe, and may fail for invalid charsets.
unsafeLoad :: Charset -> ICU.Converter
unsafeLoad c = unsafePerformIO $ ICU.open (charsetName c) (Just True)

-- | Convert a Unicode string into a codepage string.
fromUnicode :: Charset -> Text -> ByteString
fromUnicode = ICU.fromUnicode . unsafeLoad

-- | Convert a codepage string into a Unicode string.
toUnicode :: Charset -> ByteString -> Text
toUnicode = ICU.toUnicode . unsafeLoad
