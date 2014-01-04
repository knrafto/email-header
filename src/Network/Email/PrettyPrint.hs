-- | A simple pretty printer, based on Philip Walder.
module Network.Email.PrettyPrint
    ( -- * Documents
      Doc
      -- * Primitives
    , prim
    , union
    , group
    , flatten
    , options
      -- * Text
    , builder
    , word8
    , byteString
      -- * Whitespace
    , space
    , line
    , linebreak
    , softline
    , softbreak
    , hardbreak
      -- * Operators
    , (<+>)
    , above
    , (</>)
    , aboveBreak
    , (<//>)
      -- * Lists
    , hsep
    , vsep
    , fillSep
    , sep
      -- * Rendering
    , RenderOptions(..)
    , render
    ) where

import           Data.Monoid
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import           Data.ByteString.Lazy.Builder (Builder)
import           Data.String
import           Data.Word

import           Network.Email.Layout         (Layout)
import qualified Network.Email.Layout         as Layout

infixr 5 </>, <//>, `above`, `aboveBreak`
infixr 6 <+>

-- | A formatted document.
data Doc
    = Empty
    | Prim Bool (Bool -> Layout)
    | Cat Doc Doc
    | Union Doc Doc
    | Options (RenderOptions -> Doc)

instance Monoid Doc where
    mempty  = Empty
    mappend = Cat

instance IsString Doc where
    fromString = byteString . B8.pack

-- | Construct a primitive document from a layout function. Its parameter
-- indicates whether the containing group is laid out horizontally instead of
-- vertically.
prim :: (Bool -> Layout) -> Doc
prim = Prim False

-- | Use the first document if the current line will fit on the page.
-- Otherwise, use the second document.
union :: Doc -> Doc -> Doc
union = Union

-- | Specify an alternative layout with all line breaks flattened.
group :: Doc -> Doc
group x = union (flatten x) x

-- | Flatten a document to a horizontal layout.
flatten :: Doc -> Doc
flatten Empty       = Empty
flatten (Prim _ f)  = Prim True f
flatten (Cat x y)   = Cat (flatten x) (flatten y)
flatten (Union x _) = x
flatten (Options f) = Options (flatten . f)

-- | Construct a document using the given 'RenderOptions'.
options :: (RenderOptions -> Doc) -> Doc
options = Options

-- | Construct a document from a 'Builder' with the given length.
builder :: Int -> Builder -> Doc
builder k s = prim $ \_ -> Layout.builder k s

-- | Construct a document from a 'Word8'.
word8 :: Word8 -> Doc
word8 w = prim $ \_ -> Layout.word8 w

-- | Construct a document from a 'B.ByteString'.
byteString :: B.ByteString -> Doc
byteString s = prim $ \_ -> Layout.byteString s

-- | Create a newline an indent.
feed :: RenderOptions -> Layout
feed r = Layout.byteString (newline r)
      <> Layout.break 0
      <> Layout.byteString (indent r)

-- | A space character.
space :: Doc
space = word8 32

-- | A newline or space.
line :: Doc
line = options $ \r -> prim (\h -> if h then Layout.word8 32 else feed r)

-- | A newline or 'mempty'.
linebreak :: Doc
linebreak = options $ \r -> prim (\h -> if h then mempty else feed r)

-- | A space if the resulting output fits on the page, and a newline otherwise.
softline :: Doc
softline = group line

-- | 'mempty' if the resulting output fits on the page, and a newline otherwise.
softbreak :: Doc
softbreak = group linebreak

-- | A newline.
hardbreak :: Doc
hardbreak = options $ \r -> prim (\_ -> feed r)

-- | Concatenate with a 'space' in between.
(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> space <> y

-- | Concatenate with a 'line' in between.
above :: Doc -> Doc -> Doc
above x y = x <> line <> y

-- | Concatenate with a 'softline' in between.
(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

-- | Concatenate with a 'linebreak' in between.
aboveBreak :: Doc -> Doc -> Doc
aboveBreak x y = x <> linebreak <> y

-- | Concatenate with a 'softbreak' in between.
(<//>) :: Doc -> Doc -> Doc
x <//> y = x <> softbreak <> y

-- | Concatenate with an operator for a non-empty list.
fold :: Monoid a => (a -> a -> a) -> [a] -> a
fold _ [] = mempty
fold f xs = foldr1 f xs

-- | Concatenate horizontally with @(\<+\>)@.
hsep :: [Doc] -> Doc
hsep = fold (<+>)

-- | Concatenate vertically with 'above'.
vsep :: [Doc] -> Doc
vsep = fold above

-- | Concatenate with either @(\<+\>)@ or 'linebreak's.
sep :: [Doc] -> Doc
sep = group . vsep

-- | Concatenates with @(\</\>)@ as long as its fits the page, than
-- inserts a @line@ and continues doing that for all documents in
-- @xs@.
fillSep :: [Doc] -> Doc
fillSep = fold (</>)

-- | Rendering options.
data RenderOptions = RenderOptions
    { lineWidth :: Int
    , newline   :: B.ByteString
    , indent    :: B.ByteString
    }

-- | Render a document to a 'Builder'.
render :: RenderOptions -> Doc -> Builder
render r doc = Layout.render (go [doc]) 0
  where
    go []     = mempty
    go (d:ds) = case d of
        Empty     -> go ds
        Prim h f  -> f h <> go ds
        Cat x y   -> go (x:y:ds)
        Union x y -> Layout.nicest width (go (x:ds)) (go (y:ds))
        Options f -> go (f r : ds)

    width = lineWidth r
