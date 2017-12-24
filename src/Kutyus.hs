module Kutyus
    ( Frame
    , Message
    , unpackFrame
    , UnpackError(..)
    , BaseFrame
    ) where

import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B

newtype MessageId = MessageId B.ByteString deriving (Eq, Show)

newtype AuthorId = AuthorId B.ByteString deriving (Eq, Show)

data ContentType =
    Blob
    deriving (Eq, Show)

type BaseMessage = Message B.ByteString

data Message a = Message
  { author :: !AuthorId
  , parent :: !(Maybe MessageId)
  , content_type :: !ContentType
  , content :: !a
  } deriving (Eq, Show)

type BaseFrame = Frame B.ByteString

data Frame a = Frame
  { version :: !Int
  , message :: Message a
  , signature :: !B.ByteString
  } deriving (Eq, Show)

data UnpackError = InvalidVersion | FrameFormatError | MessageFormatError | InvalidSignature deriving (Eq, Show)

unpackFrame :: B.ByteString -> Either UnpackError (Frame a)
unpackFrame _ = Left InvalidVersion
