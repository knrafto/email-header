-- | Charset conversions.
module Network.Email.Charset
    ( Charset
    , encode
    , decode
    ) where

import qualified Data.ByteString       as B
import qualified Data.Text             as T
import           Data.Text.ICU.Convert
import           System.IO.Unsafe

-- | A charset name.
type Charset = String

-- | Lookup an ICU codec.
lookupCodec :: Charset -> Converter
lookupCodec charset = unsafePerformIO $ open charset (Just True)

-- | Encode Unicode text using a charset.
encode :: Charset -> T.Text -> B.ByteString
encode = fromUnicode . lookupCodec

-- | Decode Unicode text using a charset.
decode :: Charset -> B.ByteString -> T.Text
decode = toUnicode . lookupCodec
