{-# LANGUAGE OverloadedStrings #-}
-- | A header-rendering type.
module Network.Email.Header.Render.Doc
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
    , commaSep
    , optional
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
import           Network.Email.Layout         (Layout)
import qualified Network.Email.Layout         as F

infixr 6 </>

-- | Rendering options.
data RenderOptions = RenderOptions
    { lineWidth :: Int
    , indent    :: Int
    , converter :: Converter
    , encoding  :: Encoding
    } deriving (Eq)

-- | Quoted-word encoding.
data Encoding = QEncoding | Base64
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Default rendering options, which uses a line width of 78, and indent of 2,
-- and utf-8 Q-encoding.
defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions
    { lineWidth = 79
    , indent    = 2
    , converter = defaultConverter
    , encoding  = QEncoding
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
    fromString s = builder (length s) (B.string8 s)

-- | Render a document with the given options and initial positions.
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

-- | Construct a primitive document from a layout function. Its parameter
-- indicates whether the containing group is laid out horizontally instead of
-- vertically.
prim :: (RenderOptions -> Bool -> Layout Builder) -> Doc
prim = Prim False

-- | Use the first document if the current line will fit on the page.
-- Otherwise, use the second document.
union :: Doc -> Doc -> Doc
union = Union

-- | Flatten a document to a horizontal layout.
flatten :: Doc -> Doc
flatten Empty       = Empty
flatten (Prim _ f)  = Prim True f
flatten (Cat x y)   = Cat (flatten x) (flatten y)
flatten (Union x _) = x

-- | Specify an alternative layout with all line breaks flattened.
group :: Doc -> Doc
group x = union (flatten x) x

-- | Construct a 'Doc' from a 'B.Builder'.
builder :: Int -> Builder -> Doc
builder k s = prim $ \_ _ -> F.span k s

-- | Construct a 'Doc' from a 'B.ByteString'.
byteString :: B.ByteString -> Doc
byteString s = builder (B.length s) (B.byteString s)

-- | Construct a 'Builder' from a 'L.Text'.
text :: L.Text -> Doc
text = byteString . LB.toStrict . L.encodeUtf8

-- | A space layout.
space :: Layout Builder
space = F.span 1 (B.char8 ' ')

-- | A newline layout.
newline :: RenderOptions -> Layout Builder
newline r =
    F.span 2 (B.byteString "\r\n") <>
    F.break 0 <>
    mconcat (replicate (indent r) space)

-- | A line break. If undone, behaves like a space.
line :: Doc
line = prim $ \r h -> if h then space else newline r

-- | A line break. If undone, behaves like `mempty`.
linebreak :: Doc
linebreak = prim $ \r h -> if h then mempty else newline r

-- | A line break or a space.
softline :: Doc
softline = group line

-- | A line break or `mempty`.
softbreak :: Doc
softbreak = group linebreak

-- | Concatenate with a 'softline' in between.
(</>) :: Doc -> Doc -> Doc
a </> b = a <> softline <> b

-- | Separate with lines or spaces.
sep :: [Doc] -> Doc
sep = group . mconcat . intersperse line

-- | @punctuate p xs@ appends @p@ to every element of @xs@ but the last.
punctuate :: Monoid a => a -> [a] -> [a]
punctuate p = go
  where
    go []     = []
    go [x]    = [x]
    go (x:xs) = x <> p : xs

-- | Separate a group with commas.
commaSep :: (a -> Doc) -> [a] -> Doc
commaSep f = sep . punctuate "," . map f

-- | Builder a 'Maybe' value.
optional :: (a -> Doc) -> Maybe a -> Doc
optional = maybe mempty
