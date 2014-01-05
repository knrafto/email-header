{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

import Control.Applicative
import Data.Attoparsec.Lazy
import Data.ByteString.Lazy.Builder
import Data.Time.Calendar
import Data.Time.LocalTime
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import           Network.Email.Format
import qualified Network.Email.Header.Parse          as P
import qualified Network.Email.Header.Builder.Format as F

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

roundTrip
    :: (Arbitrary a, Eq a, Show a)
    => String
    -> (a -> Doc)
    -> Parser a
    -> TestTree
roundTrip name r p = testProperty name $ \a ->
    forAll (choose (20, 80)) $ \w ->
    let opts = defaultRenderOptions { lineWidth = w }
        s    = toLazyByteString $ render opts (r a)
    in  case parse p s of
            Fail _ _ _ -> False
            Done _ b   -> b == a

parsers :: TestTree
parsers = testGroup "parsers"
    [ roundTrip "date-time" F.dateTime P.dateTime
    ]

main :: IO ()
main = defaultMain parsers
