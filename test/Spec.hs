{-# LANGUAGE OverloadedStrings #-}

import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B
import qualified Crypto.Sign.Ed25519 as ED
import Data.Either

import Test.Hspec
import Test.QuickCheck

import Kutyus

messageSpec :: Spec
messageSpec =
  describe "messages" $ do
    it "fails to unpack messages with versions oher than 1" $ do
      let messageWithInvalidVersion = MP.pack (0::Int, "message"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let (Left err) = unpackMessage messageWithInvalidVersion
      err `shouldBe` InvalidVersion

    it "fails to unpack messages with wrong format" $ do
      let messageWithInvalidFrame = MP.pack ("message"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let (Left err) = unpackMessage messageWithInvalidFrame
      err `shouldBe` FrameFormatError

    it "fails to unpack messages with wrong format" $ do
      let messageWithInvalidMessage = MP.pack (1::Int, "invalidmessage"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let (Left err) = unpackMessage messageWithInvalidMessage
      err `shouldBe` MessageFormatError

    it "fails to unpack messages with an invalid signature" $ do
      let validMessage = MP.pack ("authur"::B.ByteString, []::[B.ByteString], "\0"::B.ByteString, "example"::B.ByteString)
      let messageWithInvalidSignature = MP.pack (1::Int, validMessage, "invalid signature"::B.ByteString) :: B.ByteString
      let (Left err) = unpackMessage messageWithInvalidSignature
      err `shouldBe` InvalidSignature

    it "fails to unpack messages with unknown content type" $ do
      let messageWithUnknownContentType = MP.pack ("authur"::B.ByteString, []::[B.ByteString], "\1"::B.ByteString, "example"::B.ByteString)
      let messageWithInvalidSignature = MP.pack (1::Int, messageWithUnknownContentType, "invalid signature"::B.ByteString) :: B.ByteString
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

main :: IO ()
main =
  hspec $ do
    messageSpec
    idSpec
