-- | Charset conversions.
module Network.Email.Charset
    ( Converter
    , getName
    , defaultConverter
    , lookup
    , fromUnicode
    , toUnicode
    ) where

import           Prelude               hiding (lookup)

import           Control.Arrow
import           Data.Map.Lazy         (Map)
import qualified Data.Map.Lazy         as Map
import           Data.Text.ICU.Convert
import           System.IO.Unsafe

-- | A charset name, compared using fuzziness.
newtype Charset = Charset String

instance Eq Charset where
    a == b = compare a b == EQ

instance Ord Charset where
    compare (Charset a) (Charset b) = compareNames a b

-- | All charsets.
charsets :: Map Charset Converter
charsets = Map.fromList $
    map (Charset &&& load) converterNames

-- | Load an ICU converter.
load :: String -> Converter
load name = unsafePerformIO $ open name (Just True)

-- | Load the default converter.
defaultConverter :: Converter
defaultConverter = load ""

-- | Lookup an ICU converter.
lookup :: String -> Maybe Converter
lookup name = Map.lookup (Charset name) charsets
