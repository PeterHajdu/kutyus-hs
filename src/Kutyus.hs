module Kutyus
    ( Frame
    , Message
    ) where

import qualified Data.ByteString.Lazy as B

newtype MessageId = MessageId B.ByteString

newtype AuthorId = AuthorId B.ByteString

data ContentType =
    Blob

data Message a = Message
  { author :: !AuthorId
  , parent :: !(Maybe MessageId)
  , content_type :: !ContentType
  , content :: !B.ByteString
  }

data Frame a = Frame
  { version :: !Int
  , message :: Message a
  , signature :: !B.ByteString
  }
