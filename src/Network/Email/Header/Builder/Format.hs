{-# LANGUAGE OverloadedStrings #-}
module Network.Email.Header.Builder.Format
    ( -- * Date and time
      dateTime
      -- * Addresses
    , address
    , mailbox
    , mailboxList
    , recipient
    , recipientList
      -- * Message IDs
    , messageID
    , messageIDList
      -- * Text
    , phrase
    , phraseList
    , unstructured
      -- * MIME
    , mimeVersion
    , contentType
    , contentTransferEncoding
    ) where

import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy.Builder as B
import           Data.List                    (intersperse)
import           Data.Monoid
import           Data.String
import           Data.Time
import qualified Data.Text.Lazy               as L
import           Data.Time.LocalTime
import           System.Locale

import           Network.Email.Format         (Layout, Doc)
import qualified Network.Email.Format         as F
import           Network.Email.Types

infixr 6 </>

data RenderOptions = RenderOptions

-- | A header builder.
newtype Builder = Builder { runBuilder :: RenderOptions -> Doc B.Builder }

instance Monoid Builder where
    mempty      = Builder $ \_ -> mempty
    mappend a b = Builder $ \r -> runBuilder a r <> runBuilder b r

instance IsString Builder where
    fromString s = builder (length s) (B.string8 s)

-- | Construct a 'Builder' from a 'B.Builder'.
builder :: Int -> B.Builder -> Builder
builder k s = Builder $ \_ -> F.prim $ \_ -> F.span k s

-- | Construct a 'Builder' from a 'B.ByteString'.
byteString :: B.ByteString -> Builder
byteString s = builder (B.length s) (B.byteString s)

-- | Group 'Builder's.
group :: Builder -> Builder
group a = Builder $ \r -> F.group (runBuilder a r)

-- | Get the rendering options.
options :: (RenderOptions -> Builder) -> Builder
options f = Builder $ \r -> runBuilder (f r) r

-- | A line break. If undone, behaves like a space.
line :: Builder
line = undefined

-- | A line break. If undone, behaves like `mempty`.
linebreak :: Builder
linebreak = undefined

-- | A line break or a space.
softline :: Builder
softline = group line

-- | A line break or `mempty`.
softbreak :: Builder
softbreak = group linebreak

-- | Concatenate with a 'softline' in between.
(</>) :: Builder -> Builder -> Builder
a </> b = a <> softline <> b

-- | Separate with lines or spaces.
sep :: [Builder] -> Builder
sep = group . mconcat . intersperse line

-- | Format an integer.
int :: Int -> Builder
int = fromString . show

-- | @punctuate p xs@ appends @p@ to every element of @xs@ but the last.
punctuate :: Monoid a => a -> [a] -> [a]
punctuate p = go
  where
    go []     = []
    go [x]    = [x]
    go (x:xs) = x <> p : xs

-- | Separate a group with commas.
commaSep :: (a -> Builder) -> [a] -> Builder
commaSep f = sep . punctuate "," . map f

-- | Format a date and time.
dateTime :: ZonedTime -> Builder
dateTime = fromString . formatTime defaultTimeLocale rfc822DateFormat

-- | Format an address.
address :: Address -> Builder
address (Address s) = byteString s

-- | Format an address with angle brackets.
angleAddr :: Address -> Builder
angleAddr a = "<" <> address a <> ">"

-- | Format a 'Mailbox'.
mailbox :: Mailbox -> Builder
mailbox (Mailbox n a) = case n of
    Nothing   -> address a
    Just name -> phrase name </> angleAddr a

-- | Format a list of 'Mailbox'es.
mailboxList :: [Mailbox] -> Builder
mailboxList = commaSep mailbox

-- | Format a 'Recipient'.
recipient :: Recipient -> Builder
recipient (Individual m)  = mailbox m
recipient (Group name ms) = phrase name <> ":" </> mailboxList ms

-- | Format a list of 'Recipient'es.
recipientList :: [Recipient] -> Builder
recipientList = commaSep recipient

-- | Format a message identifier
messageID :: MessageID -> Builder
messageID (MessageID s) = byteString s

-- | Format a list of message identifiers.
messageIDList :: [MessageID] -> Builder
messageIDList = commaSep messageID

-- | Format a phrase.
phrase :: L.Text -> Builder
phrase = undefined

-- | Format a list of phrases.
phraseList :: [L.Text] -> Builder
phraseList = commaSep phrase

-- | Format unstructured text.
unstructured :: L.Text -> Builder
unstructured = undefined

-- | Format the MIME version.
mimeVersion ::  Int -> Int -> Builder
mimeVersion major minor = int major <> "." <> int minor

-- | Format the content type and parameters.
contentType :: MimeType -> Parameters -> Builder
contentType = undefined

-- | Format the content transfer encoding.
contentTransferEncoding :: B.ByteString -> Builder
contentTransferEncoding = byteString
