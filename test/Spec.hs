{-# LANGUAGE OverloadedStrings #-}

import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B
import Crypto.Sign.Ed25519
import Data.Either

import Test.Hspec
import Test.QuickCheck

import Kutyus

frameSpec :: Spec
frameSpec =
  describe "frames" $ do
    it "fails to unpack frames with versions oher than 1" $ do
      let messageWithInvalidVersion = MP.pack (0::Int, "message"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidVersion :: Either UnpackError BaseMessage
      maybeUnpackedMessage `shouldBe` (Left InvalidVersion)

    it "fails to unpack frames with wrong format" $ do
      let messageWithInvalidFrame = MP.pack ("message"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidFrame :: Either UnpackError BaseMessage
      maybeUnpackedMessage `shouldBe` (Left FrameFormatError)

    it "fails to unpack messages with wrong format" $ do
      let messageWithInvalidMessage = MP.pack (1::Int, "invalidmessage"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidMessage :: Either UnpackError BaseMessage
      maybeUnpackedMessage `shouldBe` (Left MessageFormatError)

    it "fails to unpack messages with an invalid signature" $ do
      let validMessage = MP.pack ("authur"::B.ByteString, []::[B.ByteString], "\0"::B.ByteString, "example"::B.ByteString)
      let messageWithInvalidSignature = MP.pack (1::Int, validMessage, "invalid signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidSignature :: Either UnpackError BaseMessage
      maybeUnpackedMessage `shouldBe` (Left InvalidSignature)

    it "fails to unpack messages with unknown content type" $ do
      let messageWithUnknownContentType = MP.pack ("authur"::B.ByteString, []::[B.ByteString], "\1"::B.ByteString, "example"::B.ByteString)
      let messageWithInvalidSignature = MP.pack (1::Int, messageWithUnknownContentType, "invalid signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidSignature :: Either UnpackError BaseMessage
      maybeUnpackedMessage `shouldBe` (Left UnknownContentType)

    it "is possible unpack a packed message" $ do
      (pubKey, privKey) <- createKeypair
      let message = Message (AuthorId $ (B.fromStrict . unPublicKey) pubKey) Nothing Blob "example"
      let frame = packMessage (B.fromStrict . unSecretKey $ privKey) message
      let (Right unpackedMessage) = unpackFrame frame
      unpackedMessage `shouldBe` message


main :: IO ()
main = hspec frameSpec
