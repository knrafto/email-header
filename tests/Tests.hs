{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import           Control.Applicative
#endif
import           Control.Exception
import qualified Data.ByteString.Char8       as B
import           Data.CaseInsensitive        (CI)
import qualified Data.CaseInsensitive        as CI
import qualified Data.Map                    as Map
import qualified Data.Text.Lazy              as L
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Test.QuickCheck
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Network.Email.Charset
import qualified Network.Email.Header.Read   as H
import qualified Network.Email.Header.Render as R
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

token :: Gen B.ByteString
token = resize 20 $ B.pack <$> listOf1 (elements ttext)  -- TODO
  where
    ttext = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!#$%&'*+-^_`{|}~"

tokenCI :: Gen (CI B.ByteString)
tokenCI = CI.mk <$> token

addrSpec :: Gen B.ByteString
addrSpec = B.concat <$> sequence [token, pure "@", token]  -- TODO

mimeVersion :: Gen (Int, Int)
mimeVersion = (,) <$> digit <*> digit
  where
    digit = choose (0, 9)

encoding :: Gen (CI B.ByteString)
encoding = elements
    ["base64", "quoted-printable", "8bit", "7bit", "binary", "x-token"]

contentType :: Gen (MimeType, Parameters)
contentType = (,) <$> arbitrary <*> params
  where
    params    = Map.fromList <$> listOf param
    param     = (,) <$> tokenCI <*> token

instance Arbitrary Address where
    arbitrary = Address <$> addrSpec

instance Arbitrary Mailbox where
    arbitrary = Mailbox <$> option phrase <*> arbitrary

instance Arbitrary Recipient where
    arbitrary = oneof
        [Individual <$> arbitrary, Group <$> phrase <*> listOf1 arbitrary]

instance Arbitrary MessageID where
    arbitrary = MessageID <$> addrSpec

instance Arbitrary MimeType where
    arbitrary = MimeType <$> tokenCI <*> tokenCI

instance Arbitrary R.RenderOptions where
    arbitrary = R.RenderOptions
        <$> choose (20, 80)
        <*> choose (1, 8)
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Charset where
    arbitrary = pure defaultCharset  -- TODO

instance Arbitrary R.Encoding where
    arbitrary = arbitraryBoundedEnum

list1 :: Arbitrary a => Gen [a]
list1 = listOf1 arbitrary

roundTrip
    :: (Eq a, Show a)
    => String
    -> Gen a
    -> (a -> (HeaderName, R.Doc))
    -> (Headers -> Either SomeException a)
    -> TestTree
roundTrip name gen renderer parser = testProperty name $
    forAll gen $ \a opts ->
    let hs = R.renderHeaders opts [renderer a]
    in  case parser hs of
            Left e  -> exception "exception" e
            Right b -> liftBool (b == a)

tests :: TestTree
tests = testGroup "round trip"
    [ -- Origination date
      roundTrip "Date"     arbitrary R.date H.date
      -- Originator
    , roundTrip "From"     list1     R.from    H.from
    , roundTrip "Sender"   arbitrary R.sender  H.sender
    , roundTrip "Reply-To" list1     R.replyTo H.replyTo
      -- Destination address
    , roundTrip "To"       list1          R.to  H.to
    , roundTrip "Cc"       list1          R.cc  H.cc
    , roundTrip "Bcc"      (option list1) R.bcc H.bcc
      -- Identification
    , roundTrip "Message-ID"  arbitrary R.messageID  H.messageID
    , roundTrip "In-Reply-To" list1     R.inReplyTo  H.inReplyTo
    , roundTrip "References"  list1     R.references H.references
      -- Informational
    , roundTrip "Subject"  text             R.subject  H.subject
    , roundTrip "Comments" text             R.comments H.comments
    , roundTrip "Keywords" (listOf1 phrase) R.keywords H.keywords
      -- Resent
    , roundTrip "Resent-Date"   arbitrary      R.resentDate      H.resentDate
    , roundTrip "Resent-From"   list1          R.resentFrom      H.resentFrom
    , roundTrip "Resent-Sender" arbitrary      R.resentSender    H.resentSender
    , roundTrip "Resent-To"     list1          R.resentTo        H.resentTo
    , roundTrip "Resent-Cc"     list1          R.resentCc        H.resentCc
    , roundTrip "Resent-Bcc"    (option list1) R.resentBcc       H.resentBcc
    , roundTrip "Resent-Message-ID" arbitrary
        R.resentMessageID H.resentMessageID
      -- MIME
    , roundTrip "MIME-Version" mimeVersion (uncurry R.mimeVersion) H.mimeVersion
    , roundTrip "Content-Type" contentType (uncurry R.contentType) H.contentType
    , roundTrip "Content-Transfer-Encoding" encoding
        R.contentTransferEncoding H.contentTransferEncoding
    , roundTrip "Content-ID"   arbitrary   R.contentID             H.contentID
    ]

main :: IO ()
main = defaultMain tests
