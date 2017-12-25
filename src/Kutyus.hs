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
    ) where

import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Maybe
import Crypto.Sign.Ed25519

newtype MessageId = MessageId {raw :: B.ByteString} deriving (Eq, Show)

newtype AuthorId = AuthorId {publicKey :: B.ByteString} deriving (Eq, Show)

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

unpackMessage :: B.ByteString -> Either UnpackError BaseMessage
unpackMessage = unpackRawFrame >=> checkVersion >=> unpackRawMessage >=> checkSignature >=> (Right . message)

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
  (\ctype -> Message (AuthorId author) (MessageId <$> listToMaybe parent) ctype content) <$> parseContentType contentType

parseContentType :: B.ByteString -> Either UnpackError ContentType
parseContentType "\0" = Right Blob
parseContentType _ = Left UnknownContentType

serializeContentType :: ContentType -> B.ByteString
serializeContentType Blob = "\0"

checkSignature :: (RawFrame, BaseFrame) -> Either UnpackError BaseFrame
checkSignature ((_, rawMessage, signature), base) = let key = PublicKey $ B.toStrict (publicKey . author . message $ base)
                                                        sig = Signature $ B.toStrict signature
                                                        msg = B.toStrict rawMessage
                                                     in if dverify key msg sig
                                                        then Right base
                                                        else Left InvalidSignature

packMessage :: B.ByteString -> Message B.ByteString -> B.ByteString
packMessage privKey message = let packedMessage = serializeMessage message :: B.ByteString
                                  signature = signMessage privKey packedMessage :: B.ByteString
                               in MP.pack (1::Int, packedMessage::B.ByteString, signature::B.ByteString)

signMessage :: B.ByteString -> B.ByteString -> B.ByteString
signMessage privKey rawMessage = let key = (SecretKey $ B.toStrict privKey)
                                     msg = (B.toStrict rawMessage)
                                     sig = dsign key msg
                                  in B.fromStrict $ unSignature sig


serializeMessage :: Message B.ByteString -> B.ByteString
serializeMessage msg = MP.pack
  ( (publicKey $ author msg)
  , (maybeToList $ raw <$> (parent msg))
  , (serializeContentType $ content_type msg)
  , content msg)

generateKeypair :: IO (B.ByteString, B.ByteString)
generateKeypair = do
  (pub, priv) <- createKeypair
  return ((B.fromStrict $ unPublicKey pub), (B.fromStrict $ unSecretKey priv))

