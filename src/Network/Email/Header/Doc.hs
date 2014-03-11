{-# LANGUAGE OverloadedStrings #-}
-- | Header formatting and pretty-printing.
module Network.Email.Header.Doc
    ( -- * Rendering options
      RenderOptions(..)
    , Encoding(..)
    , defaultRenderOptions
      -- * Rendering
    , Doc
    , render
      -- * Construction
    , prim
    , group
    , builder
    , string
    , byteString
    , text
      -- * Spacing
    , space
    , newline
    , line
    , linebreak
    , softline
    , softbreak
    , (</>)
      -- * Combinators
    , sep
    , punctuate
    ) where

import qualified Data.ByteString              as B
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy         as LB
import           Data.List                    (intersperse)
import           Data.Monoid
import           Data.String
import qualified Data.Text.Lazy               as L
import qualified Data.Text.Lazy.Encoding      as L

import           Network.Email.Charset
import           Network.Email.Header.Layout  (Layout)
import qualified Network.Email.Header.Layout  as F

infixr 6 </>

-- | Rendering options.
data RenderOptions = RenderOptions
    { -- | The maximum line width.
      lineWidth :: Int
      -- | The indent of each line, in spaces.
    , indent    :: Int
      -- | The charset used to encode text outside US-ASCII range.
    , charset   :: Charset
      -- | The header encoding used for encoded words.
    , encoding  :: Encoding
    } deriving (Eq, Show)

-- | The encoding used for binary characters in an encoded word.
data Encoding
    -- | Quoted-printable encoding. Spaces are represented with underscores,
    -- and undisplayable characters are represented as hex pairs.
    = QP
    -- | Base 64 encoding of all characters.
    | Base64
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Default rendering options, which uses a line width of 80, and indent of 2,
-- and utf-8 quated-printable encoding.
defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions
    { lineWidth = 80
    , indent    = 2
    , charset   = defaultCharset
    , encoding  = QP
    }

-- | A formatted email header.
data Doc
    = Empty
    | Prim Bool (RenderOptions -> Bool -> Layout Builder)
    | Cat Doc Doc
    | Union Doc Doc

instance Monoid Doc where
    mempty  = Empty
    mappend = Cat

instance IsString Doc where
    fromString = string

-- | Render a document with the given options and initial position.
render :: RenderOptions -> Int -> Doc -> Builder
render r i doc = F.layout i (go [doc])
  where
    w         = lineWidth r

    go []     = mempty
    go (d:ds) = case d of
        Empty     -> go ds
        Prim h f  -> f r h <> go ds
        Cat x y   -> go (x:y:ds)
        Union x y -> F.nicest w (go (x:ds)) (go (y:ds))

-- | Construct a primitive document from a layout function. The function takes
-- two parameters: the rendering options, and a 'Bool' which indicates
-- whether the containing group is laid out horizontally instead of vertically.
prim :: (RenderOptions -> Bool -> Layout Builder) -> Doc
prim = Prim False

-- | Flatten a layout, removing all line breaks.
flatten :: Doc -> Doc
flatten Empty       = Empty
flatten (Prim _ f)  = Prim True f
flatten (Cat x y)   = Cat (flatten x) (flatten y)
flatten (Union x _) = x

-- | Specify an alternative layout with all line breaks flattened.
group :: Doc -> Doc
group x = Union (flatten x) x

-- | Construct a 'Doc' from a 'B.Builder' and a length.
builder :: Int -> Builder -> Doc
builder k s = prim $ \_ _ -> F.span k s

-- | Construct a 'Doc' from a 'String'.
string :: String -> Doc
string s = builder (length s) (B.string8 s)

-- | Construct a 'Doc' from a 'B.ByteString'.
byteString :: B.ByteString -> Doc
byteString s = builder (B.length s) (B.byteString s)

-- | Construct a 'Builder' from a 'L.Text'.
text :: L.Text -> Doc
text = byteString . LB.toStrict . L.encodeUtf8

-- | A space layout.
space :: Layout Builder
space = F.span 1 (B.char8 ' ')

-- | A newline layout. This will emit a @CRLF@ pair, break to a new line,
-- and indent.
newline :: RenderOptions -> Layout Builder
newline r =
    F.span 2 (B.byteString "\r\n") <>
    F.break 0 <>
    mconcat (replicate1 (indent r) space)
  where
    replicate1 n a = a : replicate (n - 1) a

-- | A line break. If undone, behaves like a space.
line :: Doc
line = prim $ \r h -> if h then space else newline r

-- | A line break. If undone, behaves like `mempty`.
linebreak :: Doc
linebreak = prim $ \r h -> if h then mempty else newline r

-- | A space if the remaining layout fits, and a line break otherwise.
softline :: Doc
softline = group line

-- | `mempty` if the remaining layout fits, and a line break otherwise.
softbreak :: Doc
softbreak = group linebreak

-- | Concatenate with a 'softline' in between.
(</>) :: Doc -> Doc -> Doc
a </> b = a <> softline <> b

-- | Separate a list with spaces if it fits. Otherwise, separate with lines.
sep :: [Doc] -> Doc
sep = group . mconcat . intersperse line

-- | @punctuate p xs@ appends @p@ to every element of @xs@ but the last.
punctuate :: Monoid a => a -> [a] -> [a]
punctuate p = go
  where
    go []     = []
    go [x]    = [x]
    go (x:xs) = x <> p : go xs
