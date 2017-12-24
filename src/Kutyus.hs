module Kutyus
    ( Frame
    , Message
    ) where

import qualified Data.ByteString.Lazy as B

newtype MessageId = MessageId B.ByteString

newtype AuthorId = AuthorId B.ByteString

data ContentType =
    Blob

type BaseMessage = Message B.ByteString

data Message a = Message
  { author :: !AuthorId
  , parent :: !(Maybe MessageId)
  , content_type :: !ContentType
  , content :: !a
  }

data Frame a = Frame
  { version :: !Int
  , message :: Message a
  , signature :: !B.ByteString
  }
