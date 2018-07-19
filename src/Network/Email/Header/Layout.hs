-- | A layout that keeps track of line positions.
module Network.Email.Header.Layout
    ( Layout
    , layout
    , span
    , break
    , position
    , nicest
    ) where

import Prelude     hiding (span, break)

import Data.Monoid

type LayoutStep a = Int -> (Int -> Bool, a)

-- | An abstract type representing a lazy layout.
newtype Layout a = Layout { runLayout :: LayoutStep a -> LayoutStep a }

instance Semigroup (Layout a) where
    (<>) a b = Layout $ runLayout a . runLayout b

instance Monoid (Layout a) where
    mempty      = Layout id
    mappend     = (<>)

-- | Run a layout with an initial position.
layout :: Monoid a => Int -> Layout a -> a
layout p l = snd $ runLayout l (const (const True, mempty)) p

-- | Layout an element of a given length.
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
