{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

import Control.Applicative
import Data.Attoparsec.Lazy
import Data.ByteString.Lazy.Builder
import Data.Monoid
import Data.String
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Network.Email.Header.Parse as P

infixl 3 <+>

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

instance IsString Builder where
    fromString = string8

instance IsString a => IsString (Gen a) where
    fromString = pure . fromString

(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 mappend

sometimes :: Monoid a => Gen a -> Gen a
sometimes gen = oneof [pure mempty, gen]

rarely :: Monoid a => Gen a -> Gen a
rarely gen = frequency [(9, pure mempty), (1, gen)]

fws :: Gen Builder
fws = " " <+> rarely ("\r\n" <+> fws)

cfws :: Gen Builder
cfws = fws  -- TODO

ocfws :: Gen Builder
ocfws = sometimes cfws

pad :: Gen Builder -> Gen Builder
pad gen = ocfws <+> gen <+> ocfws

dateTime :: ZonedTime -> Gen Builder
dateTime t = sometimes (dayOfWeek <+> ",") <+> date <+> time
  where
    format str = pad . fromString $ formatTime defaultTimeLocale str t

    dayOfWeek  = format "%a"
    date       = day <+> month <+> year <+> cfws
    day        = oneof [format "%d", format "%e"]
    month      = format "%b"

    -- TODO: 3-digit years
    year       = oneof $
        [ format "%0Y" ] ++
        [ format "%y" | 1950 <= y && y < 2000 ] ++
        [ format "%y" | 2000 <= y && y < 2050 ]
      where
        (y,_,_) = toGregorian . localDay $ zonedTimeToLocalTime t

    time       = timeOfDay <+> zone
    timeOfDay  = hour <+> minute <+> second
    hour       = format "%H"
    minute     = ":" <+> format "%M"
    second
        | s == (0 :: Int) = ocfws
        | otherwise       = ":" <+> format "%S"
      where
        s = floor . todSec . localTimeOfDay $ zonedTimeToLocalTime t

    -- TODO: obs-zone
    zone = format "%z"

checkParser
    :: (Arbitrary a, Eq a, Show a)
    => Parser a
    -> (a -> Gen Builder)
    -> Property
checkParser p f =
    property $ \a ->
    forAll (toLazyByteString <$> f a) $ \s ->
    case parse (P.cfws *> p <* endOfInput) s of
        Fail _ _ _ -> False
        Done _ r   -> r == a

testParser
    :: (Arbitrary a, Eq a, Show a)
    => String
    -> Parser a
    -> (a -> Gen Builder)
    -> TestTree
testParser name p f = testProperty name (checkParser p f)

parsers :: TestTree
parsers = testGroup "parsers"
    [ testParser "date-time" P.dateTime dateTime
    ]

main :: IO ()
main = defaultMain parsers
