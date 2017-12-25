module Kutyus
    ( Frame
    , Message
    , unpackFrame
    , UnpackError(..)
    , BaseFrame
    ) where

import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Maybe

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

type RawFrame = (Int, B.ByteString, B.ByteString)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just val) = Right val
maybeToEither err Nothing = Left err

unpackFrame :: B.ByteString -> Either UnpackError BaseFrame
unpackFrame = unpackRawFrame >=> checkVersion >=> unpackMessage >=> checkSignature >=> undefined

unpackRawFrame :: B.ByteString -> Either UnpackError RawFrame
unpackRawFrame buffer = let maybeRawFrame = MP.unpack buffer :: Maybe RawFrame
                         in maybeToEither FrameFormatError maybeRawFrame

checkVersion :: RawFrame -> Either UnpackError RawFrame
checkVersion frame@(1, _, _) = Right frame
checkVersion (_, _, _) = Left InvalidVersion

type RawMessage = (B.ByteString, [B.ByteString], B.ByteString, B.ByteString)
unpackMessage :: RawFrame -> Either UnpackError BaseFrame
unpackMessage (version, message, signature) = let maybeRawMessage = MP.unpack message :: Maybe RawMessage
                                                  maybeBaseMessage = unpackRawMessage <$> maybeRawMessage
                                                  maybeBaseFrame = (\msg -> Frame version msg signature) <$> maybeBaseMessage
                                               in maybeToEither MessageFormatError maybeBaseFrame

unpackRawMessage :: RawMessage -> BaseMessage
unpackRawMessage (author, parent, contentType, content) = Message (AuthorId author) (MessageId <$> listToMaybe parent) (parseContentType contentType) content

parseContentType :: B.ByteString -> ContentType
parseContentType _ = Blob

checkSignature :: BaseFrame -> Either UnpackError BaseFrame
checkSignature _ = Left InvalidSignature
