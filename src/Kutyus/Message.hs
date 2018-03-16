{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Kutyus.Message
  ( Message(..)
  , unpackMessage
  , packMessage
  , UnpackError(..)
  , BaseMessage
  , ContentType(..)
  , AuthorId(..)
  , generateKeypair
  , MessageId(..)
  , PublicKey(..)
  , PrivateKey(..)
  , messageIdToBase64
  , base64ToMessageId
  , authorIdToBase64
  , base64ToAuthorId
  , Base64(..)
  ) where

import Kutyus.Crypto
import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Base64 as B64

newtype Base64 = Base64 {rawBase64 :: S.ByteString} deriving (Eq, Show)
newtype MessageId = MessageId {raw :: S.ByteString} deriving (Eq, Show)

messageIdToBase64 :: MessageId -> Base64
messageIdToBase64 (MessageId raw) = Base64 $ B64.encode raw

base64ToMessageId :: Base64 -> Maybe MessageId
base64ToMessageId (Base64 raw) = MessageId <$> (eitherToMaybe $ B64.decode raw)

authorIdToBase64 :: AuthorId -> Base64
authorIdToBase64 (AuthorId (PublicKey raw)) = Base64 $ B64.encode raw

base64ToAuthorId :: Base64 -> Maybe AuthorId
base64ToAuthorId (Base64 raw) = (AuthorId . PublicKey) <$> (eitherToMaybe $ B64.decode raw)

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

data FrameState = Unchecked | ValidFrame

data Frame (fs :: FrameState) a = Frame
  { version :: !Int
  , message :: Message a
  , messageId :: !MessageId
  , signature :: !Signature
  } deriving (Eq, Show)

data UnpackError =
    InvalidVersion
  | FrameFormatError
  | MessageFormatError
  | InvalidSignature
  | UnknownContentType deriving (Eq, Show)


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right val) = Just val

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just val) = Right val
maybeToEither err Nothing = Left err

unpackMessage :: L.ByteString -> Either UnpackError (MessageId, BaseMessage)
unpackMessage = unpackRawFrame >=> checkVersion >=> unpackRawMessage >=> checkSignature >=> constructMessageAndId

type BaseFrame fs = Frame fs L.ByteString
constructMessageAndId :: BaseFrame 'ValidFrame -> Either UnpackError (MessageId, BaseMessage)
constructMessageAndId frame = Right $ (messageId frame, message frame)

type RawFrame = (Int, L.ByteString, L.ByteString)
unpackRawFrame :: L.ByteString -> Either UnpackError RawFrame
unpackRawFrame buffer = let maybeRawFrame = MP.unpack buffer :: Maybe RawFrame
                         in maybeToEither FrameFormatError maybeRawFrame

checkVersion :: RawFrame -> Either UnpackError RawFrame
checkVersion frame@(1, _, _) = Right frame
checkVersion (_, _, _) = Left InvalidVersion

type RawMessage = (L.ByteString, [S.ByteString], L.ByteString, L.ByteString)
unpackRawMessage :: RawFrame -> Either UnpackError (BaseFrame Unchecked)
unpackRawMessage rawFrame@(version, message, signature) = let sig = sigFromLazy signature
                                                              eitherRawMessage = maybeToEither MessageFormatError (MP.unpack message :: Maybe RawMessage)
                                                              msgId = MessageId . rawDigest $ digest message
                                                              eitherBaseMessage = eitherRawMessage >>= parseRawMessage


                                                           in (\msg -> Frame version msg msgId sig) <$> eitherBaseMessage

parseRawMessage :: RawMessage -> Either UnpackError BaseMessage
parseRawMessage (author, parent, contentType, content) =
  (\ctype -> Message (AuthorId $ pubKeyFromLazy author) (MessageId <$> listToMaybe parent) ctype content) <$> parseContentType contentType

parseContentType :: L.ByteString -> Either UnpackError ContentType
parseContentType "\0" = Right Blob
parseContentType _ = Left UnknownContentType

serializeContentType :: ContentType -> S.ByteString
serializeContentType Blob = "\0"

checkSignature :: BaseFrame 'Unchecked -> Either UnpackError (BaseFrame 'ValidFrame)
checkSignature base@(Frame v m mid s) = let key = publicKey . author . message $ base
                      in if verifySignature key (signature base) (Digest $ raw $ messageId base)
                         then Right (Frame v m mid s)
                         else Left InvalidSignature

packMessage :: PrivateKey -> Message L.ByteString -> (MessageId, L.ByteString)
packMessage privKey message = let packedMessage = serializeMessage message
                                  msgDigest = digest packedMessage
                                  signature = sign privKey msgDigest
                                  msgId = MessageId $ rawDigest msgDigest
                               in (msgId, MP.pack (1::Int, packedMessage::L.ByteString, (rawSignature signature)::S.ByteString))

serializeMessage :: Message L.ByteString -> L.ByteString
serializeMessage msg = MP.pack
  ( (rawPublicKey $ publicKey $ author msg)
  , (maybeToList $ raw <$> (parent msg))
  , (serializeContentType $ content_type msg)
  , content msg
  )

