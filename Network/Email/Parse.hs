{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of common header values.
--
-- The parsers exported here are lexeme parsers; that is, they consume
-- comments or whitespace that follows, but expect no leading comments or
-- whitespace. This makes combining parsers easier and more efficient. The
--  only place whitespace is explicitly skipped is at the beginning of a
-- string, and this is done automatically by 'parseHeader'.
module Network.Email.Parse
    ( module Network.Email.Parse.Address
    , module Network.Email.Parse.DateTime
    , module Network.Email.Parse.MessageId
    , module Network.Email.Parse.Text
    ) where

import Network.Email.Parse.Address
import Network.Email.Parse.DateTime
import Network.Email.Parse.MessageId
import Network.Email.Parse.Text
