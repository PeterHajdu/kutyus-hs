{-# LANGUAGE OverloadedStrings #-}

module Kutyus
    ( Frame
    , Message(..)
    , unpackMessage
    , packMessage
    , UnpackError(..)
    , BaseMessage
    , ContentType(..)
    , AuthorId(..)
    , generateKeypair
    , MessageId
    , PublicKey(..)
    , PrivateKey(..)
    ) where

import Kutyus.Crypto
import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Control.Monad
import Data.Maybe

newtype MessageId = MessageId {raw :: S.ByteString} deriving (Eq, Show)

newtype AuthorId = AuthorId {publicKey :: PublicKey} deriving (Eq, Show)

data ContentType =
    Blob
    deriving (Eq, Show)

type BaseMessage = Message L.ByteString

data Message a = Message
  { author :: !AuthorId
  , parent :: !(Maybe MessageId)
  , content_type :: !ContentType
  , content :: !a
  } deriving (Eq, Show)

type BaseFrame = Frame L.ByteString

data Frame a = Frame
  { version :: !Int
  , message :: Message a
  , signature :: Signature
  } deriving (Eq, Show)

data UnpackError =
    InvalidVersion
  | FrameFormatError
  | MessageFormatError
  | InvalidSignature
  | UnknownContentType deriving (Eq, Show)

type RawFrame = (Int, L.ByteString, L.ByteString)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just val) = Right val
maybeToEither err Nothing = Left err

unpackMessage :: L.ByteString -> Either UnpackError (MessageId, BaseMessage)
unpackMessage = unpackRawFrame >=> checkVersion >=> unpackRawMessage >=> checkSignature >=> constructMessageAndId

constructMessageAndId :: (MessageId, BaseFrame) -> Either UnpackError (MessageId, BaseMessage)
constructMessageAndId (msgId, frame) = Right $ (msgId, message frame)

unpackRawFrame :: L.ByteString -> Either UnpackError RawFrame
unpackRawFrame buffer = let maybeRawFrame = MP.unpack buffer :: Maybe RawFrame
                         in maybeToEither FrameFormatError maybeRawFrame

checkVersion :: RawFrame -> Either UnpackError RawFrame
checkVersion frame@(1, _, _) = Right frame
checkVersion (_, _, _) = Left InvalidVersion

type RawMessage = (L.ByteString, [S.ByteString], L.ByteString, L.ByteString)
unpackRawMessage :: RawFrame -> Either UnpackError (RawFrame, BaseFrame)
unpackRawMessage rawFrame@(version, message, signature) = let sig = sigFromLazy signature
                                                              eitherRawMessage = maybeToEither MessageFormatError (MP.unpack message :: Maybe RawMessage)
                                                              eitherBaseMessage = eitherRawMessage >>= parseRawMessage
                                                           in (\msg -> (rawFrame, Frame version msg sig)) <$> eitherBaseMessage

parseRawMessage :: RawMessage -> Either UnpackError BaseMessage
parseRawMessage (author, parent, contentType, content) =
  (\ctype -> Message (AuthorId $ pubKeyFromLazy author) (MessageId <$> listToMaybe parent) ctype content) <$> parseContentType contentType

parseContentType :: L.ByteString -> Either UnpackError ContentType
parseContentType "\0" = Right Blob
parseContentType _ = Left UnknownContentType

serializeContentType :: ContentType -> S.ByteString
serializeContentType Blob = "\0"

checkSignature :: (RawFrame, BaseFrame) -> Either UnpackError (MessageId, BaseFrame)
checkSignature ((_, rawMessage, _), base) = let key = publicKey . author . message $ base
                                                msgId = digest rawMessage
                                             in if verifySignature key (signature base) msgId
                                                then Right (MessageId $ rawDigest msgId, base)
                                                else Left InvalidSignature

packMessage :: PrivateKey -> Message L.ByteString -> L.ByteString
packMessage privKey message = let packedMessage = serializeMessage message
                                  signature = sign privKey (digest packedMessage)
                               in MP.pack (1::Int, packedMessage::L.ByteString, (rawSignature signature)::S.ByteString)

serializeMessage :: Message L.ByteString -> L.ByteString
serializeMessage msg = MP.pack
  ( (rawPublicKey $ publicKey $ author msg)
  , (maybeToList $ raw <$> (parent msg))
  , (serializeContentType $ content_type msg)
  , content msg
  )

