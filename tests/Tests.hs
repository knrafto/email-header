{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.String
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Network.Email.Parse.Header.DateTime as P
import Network.Email.Parse.Header.Internal
import qualified Network.Email.Render.Header.DateTime as R

import           Render (Render(..), Doc)
import qualified Render as R

fws :: Doc
fws = R.oneof [" ", "  "]

cfws :: Doc
cfws = R.frequency [(9, fws), (1, "(comment)")]

ocfws :: Doc
ocfws = R.option cfws

checkParser :: (Arbitrary a, Render a, Eq a, Show a) => Parser a -> Property
checkParser p = property $ \a ->
    forAll (R.renderDoc $ render a) $ \s ->
    case parseOnly (skipCfws *> p) s of
        Left  _ -> False
        Right r -> r == a

checkRenderer
    :: (Arbitrary a, Eq a, Show a)
    => Parser a
    -> (a -> Builder)
    -> Property
checkRenderer p r = property $ \a ->
    case parseOnly (skipCfws *> p) (L.toStrict . toLazyByteString $ r a) of
        Left  _ -> False
        Right r -> r == a

instance Eq ZonedTime where
    ZonedTime t1 z1 == ZonedTime t2 z2 = (t1, z1) == (t2, z2)

instance Arbitrary ZonedTime where
    arbitrary = ZonedTime <$> local <*> zone
      where
        local = do
            h <- choose (0, 23)
            m <- choose (0, 59)
            s <- fromInteger <$> choose (0, 60)
            d <- choose (0, 50000)
            return $ LocalTime (ModifiedJulianDay d) (TimeOfDay h m s)

        zone = minutesToTimeZone <$> choose (-12*60, 14*60)

instance Render ZonedTime where
    render t = R.option (ocfws <> format "%a" <> ocfws <> ",")
            <> ocfws <> R.oneof [format "%d", format "%e"]
            <> cfws <> format "%b"
            <> cfws <> year
            <> cfws <> format "%H"
            <> ocfws <> ":" <> ocfws <> format "%M"
            <> ocfws <> seconds
            -- TODO: obs-zone
            <> cfws <> format "%z"
            <> ocfws
      where
        format str = fromString $ formatTime defaultTimeLocale str t

        -- TODO: 3-digit years
        year = R.oneof $
            [ format "%0Y" ] ++
            [ format "%y" | 1950 <= y && y < 2000 ] ++
            [ format "%y" | 2000 <= y && y < 2050 ]
          where
            (y,_,_) = toGregorian . localDay $ zonedTimeToLocalTime t

        seconds
            | s == (0 :: Int) = mempty
            | otherwise       = ":" <> ocfws <> format "%S"
          where
            s = floor . todSec . localTimeOfDay $ zonedTimeToLocalTime t

tests :: TestTree
tests = testGroup "tests" [parsers, renderers]

parsers :: TestTree
parsers = testGroup "parsers"
    [ testProperty "date-time" $ checkParser P.dateTime
    ]

renderers :: TestTree
renderers = testGroup "renderers"
    [ testProperty "date-time" $ checkRenderer P.dateTime R.dateTime
    ]

main :: IO ()
main = defaultMain tests
