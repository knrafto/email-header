-- | A simple pretty printer, based on Philip Walder.
module Network.Email.Format
    ( -- * Layouts
      Layout
    , layout
    , span
    , break
    , position
    , nicest
      -- * Documents
    , Doc
    , render
    , prim
    , union
    , group
    , flatten
    ) where

import Prelude     hiding (span, break)

import Data.Monoid

type LayoutStep a = Int -> (Int -> Bool, a)

-- | An abstract type representing a lazy layout.
newtype Layout a = Layout { runLayout :: LayoutStep a -> LayoutStep a }

instance Monoid (Layout a) where
    mempty      = Layout id
    mappend a b = Layout $ runLayout a . runLayout b

-- | Run a layout with an initial position to produce a rendered 'Builder'.
layout :: Monoid a => Layout a -> Int -> a
layout l p = snd (runLayout l (\_ -> (const True, mempty)) p)

-- | Layout a an element of a given length.
span :: Monoid a => Int -> a -> Layout a
span k s = Layout $ \c p ->
    let ~(fits, b) = c (p + k)
    in  (\w -> p <= w && fits w, s <> b)

-- | Layout a new line and set the initial position.
break :: Int -> Layout a
break i = Layout $ \c p ->
    let ~(_, b) = c i
    in  (\w -> p <= w, b)

-- | Use the current line position to produce a layout.
position :: (Int -> Layout a) -> Layout a
position f = Layout $ \c p -> runLayout (f p) c p

-- | Choose the first layout if the first line fits within the given length,
-- and the second otherwise.
nicest :: Int -> Layout a -> Layout a -> Layout a
nicest w x y = Layout $ \c p ->
    let a@(fits, _) = runLayout x c p
        b           = runLayout y c p
    in  if fits w then a else b

-- | A formatted document.
data Doc a
    = Empty
    | Prim Bool (Bool -> Layout a)
    | Cat (Doc a) (Doc a)
    | Union (Doc a) (Doc a)

instance Monoid (Doc a) where
    mempty  = Empty
    mappend = Cat

-- | Construct a primitive document from a layout function. Its parameter
-- indicates whether the containing group is laid out horizontally instead of
-- vertically.
prim :: (Bool -> Layout a) -> Doc a
prim = Prim False

-- | Use the first document if the current line will fit on the page.
-- Otherwise, use the second document.
union :: Doc a -> Doc a -> Doc a
union = Union

-- | Specify an alternative layout with all line breaks flattened.
group :: Doc a -> Doc a
group x = union (flatten x) x

-- | Flatten a document to a horizontal layout.
flatten :: Doc a -> Doc a
flatten Empty       = Empty
flatten (Prim _ f)  = Prim True f
flatten (Cat x y)   = Cat (flatten x) (flatten y)
flatten (Union x _) = x

-- | Render a document with the given line width.
render :: Monoid a => Int -> Doc a -> Layout a
render w doc = go [doc]
  where
    go []     = mempty
    go (d:ds) = case d of
        Empty     -> go ds
        Prim h f  -> f h <> go ds
        Cat x y   -> go (x:y:ds)
        Union x y -> nicest w (go (x:ds)) (go (y:ds))
