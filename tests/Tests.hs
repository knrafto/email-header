{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Locale
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Email.Parse.Header
import           Network.Email.Types

instance Eq ZonedTime where
    ZonedTime t1 z1 == ZonedTime t2 z2 = (t1, z1) == (t2, z2)

instance Arbitrary ZonedTime where
    arbitrary = ZonedTime <$> local <*> zone
      where
        local = do
            h <- choose (0, 23)
            m <- choose (0, 59)
            s <- fromInteger <$> choose (0, 60)
            d <- (2000 +) <$> arbitrary
            return $ LocalTime (ModifiedJulianDay d) (TimeOfDay h m s)

        zone = minutesToTimeZone <$> choose (-12*60, 14*60)

tests :: TestTree
tests = testGroup "headers" [dateTests]

dateTests :: TestTree
dateTests = testGroup "date"
    [ testProperty "standard" $
        \date -> testDate date "%a, %e %b %0Y %T %z" ==
                 Right date
    , testProperty "short" $
        \date -> testDate date "%e %b %0Y %R %z" ==
                 testDate date "%a, %e %b %0Y %R:00 %z"
    , testProperty "space" $
        \date -> testDate date " %a(comment), %e %b %0Y %H : %M : %S %z " ==
                 Right date
    , testProperty "2-digit year" $
        \date -> (year date `div` 100 < 50 ==>
                  testDate date "%e %b   %y %T %z" ==
                  testDate date "%e %b 19%y %T %z")
            .||. (testDate date "%e %b   %y %T %z" ==
                  testDate date "%e %b 20%y %T %z")
    ]
  where
    testDate :: ZonedTime -> String -> Either EmailError ZonedTime
    testDate date str =
        let s = formatTime defaultTimeLocale str date
        in  dateField [("Date", L.pack s)]

    year date =
        let (y,_,_) = toGregorian . localDay $ zonedTimeToLocalTime date
        in y

main :: IO ()
main = defaultMain tests
