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

import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Maybe
import qualified Crypto.Sign.Ed25519 as ED
import Crypto.Hash.SHA512

newtype MessageId = MessageId {raw :: B.ByteString} deriving (Eq, Show)

newtype AuthorId = AuthorId {publicKey :: PublicKey} deriving (Eq, Show)

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

data UnpackError =
    InvalidVersion
  | FrameFormatError
  | MessageFormatError
  | InvalidSignature
  | UnknownContentType deriving (Eq, Show)

type RawFrame = (Int, B.ByteString, B.ByteString)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just val) = Right val
maybeToEither err Nothing = Left err

unpackMessage :: B.ByteString -> Either UnpackError (MessageId, BaseMessage)
unpackMessage = unpackRawFrame >=> checkVersion >=> unpackRawMessage >=> checkSignature >=> constructMessageAndId

constructMessageAndId :: (MessageId, BaseFrame) -> Either UnpackError (MessageId, BaseMessage)
constructMessageAndId (msgId, frame) = Right $ (msgId, message frame)

unpackRawFrame :: B.ByteString -> Either UnpackError RawFrame
unpackRawFrame buffer = let maybeRawFrame = MP.unpack buffer :: Maybe RawFrame
                         in maybeToEither FrameFormatError maybeRawFrame

checkVersion :: RawFrame -> Either UnpackError RawFrame
checkVersion frame@(1, _, _) = Right frame
checkVersion (_, _, _) = Left InvalidVersion

type RawMessage = (B.ByteString, [B.ByteString], B.ByteString, B.ByteString)
unpackRawMessage :: RawFrame -> Either UnpackError (RawFrame, BaseFrame)
unpackRawMessage rawFrame@(version, message, signature) = let eitherRawMessage = maybeToEither MessageFormatError (MP.unpack message :: Maybe RawMessage)
                                                              eitherBaseMessage = eitherRawMessage >>= parseRawMessage
                                                           in (\msg -> (rawFrame, Frame version msg signature)) <$> eitherBaseMessage

parseRawMessage :: RawMessage -> Either UnpackError BaseMessage
parseRawMessage (author, parent, contentType, content) =
  (\ctype -> Message (AuthorId $ PublicKey author) (MessageId <$> listToMaybe parent) ctype content) <$> parseContentType contentType

parseContentType :: B.ByteString -> Either UnpackError ContentType
parseContentType "\0" = Right Blob
parseContentType _ = Left UnknownContentType

serializeContentType :: ContentType -> B.ByteString
serializeContentType Blob = "\0"

checkSignature :: (RawFrame, BaseFrame) -> Either UnpackError (MessageId, BaseFrame)
checkSignature ((_, rawMessage, signature), base) = let key = ED.PublicKey $ B.toStrict (rawPublicKey . publicKey . author . message $ base)
                                                        sig = ED.Signature $ B.toStrict signature
                                                        msgHash = hashlazy rawMessage
                                                     in if ED.dverify key msgHash sig
                                                        then Right (MessageId $ B.fromStrict msgHash, base)
                                                        else Left InvalidSignature

packMessage :: PrivateKey -> Message B.ByteString -> B.ByteString
packMessage privKey message = let packedMessage = serializeMessage message :: B.ByteString
                                  signature = signMessage privKey packedMessage :: B.ByteString
                               in MP.pack (1::Int, packedMessage::B.ByteString, signature::B.ByteString)

signMessage :: PrivateKey -> B.ByteString -> B.ByteString
signMessage privKey rawMessage = let key = (ED.SecretKey $ B.toStrict $ rawPrivateKey privKey)
                                     msgHash = hashlazy rawMessage
                                     sig = ED.dsign key msgHash
                                  in B.fromStrict $ ED.unSignature sig


serializeMessage :: Message B.ByteString -> B.ByteString
serializeMessage msg = MP.pack
  ( (rawPublicKey $ publicKey $ author msg)
  , (maybeToList $ raw <$> (parent msg))
  , (serializeContentType $ content_type msg)
  , content msg)

newtype PrivateKey = PrivateKey {rawPrivateKey :: B.ByteString} deriving (Eq, Show)
newtype PublicKey = PublicKey {rawPublicKey :: B.ByteString} deriving (Eq, Show)

generateKeypair :: IO (PublicKey, PrivateKey)
generateKeypair = do
  (pub, priv) <- ED.createKeypair
  return ((PublicKey $ B.fromStrict $ ED.unPublicKey pub), (PrivateKey $ B.fromStrict $ ED.unSecretKey priv))

