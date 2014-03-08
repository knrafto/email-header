{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

import           Control.Applicative
import qualified Data.ByteString.Char8        as B
import qualified Data.Text.Lazy               as L
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Email.Charset
import qualified Network.Email.Header.Parse   as P
import qualified Network.Email.Header.Render  as R
import           Network.Email.Header.Types

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

option :: Gen a -> Gen (Maybe a)
option gen = frequency [(1, pure Nothing), (3, Just <$> gen)]

char :: Gen Char
char = frequency
    [ (9, elements [' ' .. '~'])
    , (1, arbitrary)
    ]

text :: Gen L.Text
text = L.pack <$> listOf char

phrase :: Gen L.Text
phrase = L.pack <$> listOf1 char

addrSpec :: Gen B.ByteString
addrSpec = B.pack . concat <$> sequence [dotAtom, pure "@", dotAtom]  -- TODO
  where
    dotAtom = resize 20 . listOf1 $ elements atext
    atext   = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!#$%&'*+-/=?^_`{|}~"

instance Arbitrary Address where
    arbitrary = Address <$> addrSpec

instance Arbitrary Mailbox where
    arbitrary = Mailbox <$> option phrase <*> arbitrary

instance Arbitrary Recipient where
    arbitrary = oneof
        [Individual <$> arbitrary, Group <$> phrase <*> listOf1 arbitrary]

instance Arbitrary MessageID where
    arbitrary = MessageID <$> addrSpec

instance Arbitrary R.RenderOptions where
    arbitrary = R.RenderOptions
        <$> choose (20, 80)
        <*> choose (1, 8)
        <*> pure defaultConverter
        <*> arbitrary

instance Arbitrary Converter where
    arbitrary = pure defaultConverter  -- TODO

instance Arbitrary R.Encoding where
    arbitrary = arbitraryBoundedEnum

list1 :: Arbitrary a => Gen [a]
list1 = listOf1 arbitrary

roundTrip
    :: (Eq a, Show a)
    => String
    -> Gen a
    -> (a -> (HeaderName, R.Doc))
    -> (Headers -> Maybe a)
    -> TestTree
roundTrip name gen renderer parser = testProperty name $
    forAll gen $ \a opts ->
    let hs = R.renderHeaders opts [renderer a]
    in  case parser hs of
            Nothing -> False
            Just b  -> b == a

parsers :: TestTree
parsers = testGroup "round trip"
    [ -- Origination date
      roundTrip "Date"     arbitrary R.date P.date
      -- Originator
    , roundTrip "From"     list1     R.from    P.from
    , roundTrip "Sender"   arbitrary R.sender  P.sender
    , roundTrip "Reply-To" list1     R.replyTo P.replyTo
      -- Destination address
    , roundTrip "To"       list1          R.to  P.to
    , roundTrip "Cc"       list1          R.cc  P.cc
    , roundTrip "Bcc"      (option list1) R.bcc P.bcc
      -- Identification
    , roundTrip "Message-ID"  arbitrary R.messageID  P.messageID
    , roundTrip "In-Reply-To" list1     R.inReplyTo  P.inReplyTo
    , roundTrip "References"  list1     R.references P.references
      -- Informational
    , roundTrip "Subject"  text             R.subject  P.subject
    , roundTrip "Comments" text             R.comments P.comments
    , roundTrip "Keywords" (listOf1 phrase) R.keywords P.keywords
      -- Resent
    , roundTrip "Resent-Date"       arbitrary      R.resentDate      P.resentDate
    , roundTrip "Resent-From"       list1          R.resentFrom      P.resentFrom
    , roundTrip "Resent-Sender"     arbitrary      R.resentSender    P.resentSender
    , roundTrip "Resent-To"         list1          R.resentTo        P.resentTo
    , roundTrip "Resent-Cc"         list1          R.resentCc        P.resentCc
    , roundTrip "Resent-Bcc"        (option list1) R.resentBcc       P.resentBcc
    , roundTrip "Resent-Message-ID" arbitrary      R.resentMessageID P.resentMessageID
    ]

main :: IO ()
main = defaultMain parsers
