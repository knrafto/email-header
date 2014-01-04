-- | Layouts that track position.
module Network.Email.Layout
    ( -- * Layout type
      Layout
    , render
      -- * Construction
    , builder
    , word8
    , byteString
    , break
    , position
    , nicest
    ) where

import           Prelude                      hiding (break)

import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as Builder
import           Data.Monoid
import           Data.String
import           Data.Word

type LayoutStep = Int -> (Int -> Bool, Builder)

-- | An abstract type representing a lazy layout.
newtype Layout = Layout { runLayout :: LayoutStep -> LayoutStep }

instance Monoid Layout where
    mempty      = Layout id
    mappend a b = Layout $ runLayout a . runLayout b

instance IsString Layout where
    fromString = byteString . B8.pack

-- | Run a layout with an initial position to produce a rendered 'Builder'.
render :: Layout -> Int -> Builder
render l p = snd (runLayout l (\_ -> (const True, mempty)) p)

-- | Layout a 'Builder' of a given length.
builder :: Int -> Builder -> Layout
builder k s = Layout $ \c p ->
    let (fits, b) = c (p + k)
    in  (\w -> p <= w && fits w, s <> b)

-- | Layout a 'Word8'.
word8 :: Word8 -> Layout
word8 w = builder 1 (Builder.word8 w)

-- | Layout a 'B.ByteString'.
byteString :: B.ByteString -> Layout
byteString s = builder (B.length s) (Builder.byteString s)

-- | Layout a new line and set the initial position.
break :: Int -> Layout
break i = Layout $ \c p ->
    let (_, b) = c i
    in  (\w -> p <= w, b)

-- | Use the current line position to produce a layout.
position :: (Int -> Layout) -> Layout
position f = Layout $ \c p -> runLayout (f p) c p

-- | Choose the first layout if the first line fits within the given length,
-- and the second otherwise.
nicest :: Int -> Layout -> Layout -> Layout
nicest w x y = Layout $ \c p ->
    let a@(fits, _) = runLayout x c p
        b           = runLayout y c p
    in  if fits w then a else b
