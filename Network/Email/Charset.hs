-- | Charset conversions.
module Network.Email.Charset
    ( encode
    , decode
    ) where

import qualified Data.ByteString       as B
import           Data.Map.Lazy         (Map)
import qualified Data.Map.Lazy         as Map
import qualified Data.Text             as T
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
    map (\name -> (Charset name, load name)) converterNames

-- | Load an ICU converter.
load :: String -> Converter
load name = unsafePerformIO $ open name (Just True)

-- | Use an ICU converter.
convertWith :: (Converter -> a -> b) -> String -> a -> Maybe b
convertWith f name a = do
    converter <- Map.lookup (Charset name) charsets
    return (f converter a)

-- | Encode Unicode text using a charset, or return 'Nothing' if the charset
-- does not exist.
encode :: String -> T.Text -> Maybe B.ByteString
encode = convertWith fromUnicode

-- | Decode Unicode text using a charset, or return 'Nothing' if the charset
-- does not exist.
decode :: String -> B.ByteString -> Maybe T.Text
decode = convertWith toUnicode
