{-# LANGUAGE OverloadedStrings #-}
module MessageSpec where

import qualified Data.MessagePack as MP
import qualified Crypto.Sign.Ed25519 as ED
import qualified Data.ByteString.Lazy as L
import Data.Either
import Kutyus
import Test.Hspec

messageSpec :: Spec
messageSpec =
  describe "messages" $ do
    it "fails to unpack messages with versions oher than 1" $ do
      let messageWithInvalidVersion = MP.pack (0::Int, "message"::L.ByteString, "signature"::L.ByteString) :: L.ByteString
      let (Left err) = unpackMessage messageWithInvalidVersion
      err `shouldBe` InvalidVersion

    it "fails to unpack messages with wrong format" $ do
      let messageWithInvalidFrame = MP.pack ("message"::L.ByteString, "signature"::L.ByteString) :: L.ByteString
      let (Left err) = unpackMessage messageWithInvalidFrame
      err `shouldBe` FrameFormatError

    it "fails to unpack messages with wrong format" $ do
      let messageWithInvalidMessage = MP.pack (1::Int, "invalidmessage"::L.ByteString, "signature"::L.ByteString) :: L.ByteString
      let (Left err) = unpackMessage messageWithInvalidMessage
      err `shouldBe` MessageFormatError

    it "fails to unpack messages with an invalid signature" $ do
      let validMessage = MP.pack ("authur"::L.ByteString, []::[L.ByteString], "\0"::L.ByteString, "example"::L.ByteString)
      let messageWithInvalidSignature = MP.pack (1::Int, validMessage, "invalid signature"::L.ByteString) :: L.ByteString
      let (Left err) = unpackMessage messageWithInvalidSignature
      err `shouldBe` InvalidSignature

    it "fails to unpack messages with unknown content type" $ do
      let messageWithUnknownContentType = MP.pack ("authur"::L.ByteString, []::[L.ByteString], "\1"::L.ByteString, "example"::L.ByteString)
      let messageWithInvalidSignature = MP.pack (1::Int, messageWithUnknownContentType, "invalid signature"::L.ByteString) :: L.ByteString
      let (Left err) = unpackMessage messageWithInvalidSignature
      err `shouldBe` UnknownContentType

    it "is possible unpack a packed message" $ do
      (pubKey, privKey) <- generateKeypair
      let message = Message (AuthorId pubKey) Nothing Blob "example"
      let (_, frame) = packMessage privKey message
      let (Right (_, unpackedMessage)) = unpackMessage frame
      unpackedMessage `shouldBe` message

idSpec :: Spec
idSpec =
  describe "ids" $ do
    let rawBytestring = "32j0wejfwiejfoj"
    let expectedBase64 = Base64 "MzJqMHdlamZ3aWVqZm9q"

    describe "messageid" $ do
      it "can be converted to base64" $ do
        expectedBase64 `shouldBe` (messageIdToBase64 $ MessageId rawBytestring)
      it "can be created from base64" $ do
        (Just $ MessageId rawBytestring) `shouldBe` (base64ToMessageId expectedBase64)

    describe "authorid" $ do
      it "can be converted to base64" $ do
        expectedBase64 `shouldBe` (authorIdToBase64 $ AuthorId $ PublicKey rawBytestring)
      it "can be created from base64" $ do
        (Just $ AuthorId $ PublicKey rawBytestring) `shouldBe` (base64ToAuthorId expectedBase64)

