{-# LANGUAGE OverloadedStrings #-}

import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B

import Test.Hspec

import Kutyus

frameSpec :: Spec
frameSpec =
  describe "frames" $ do
    it "fails to unpack frames with versions oher than 1" $ do
      let messageWithInvalidVersion = MP.pack (0::Int, "message"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidVersion :: Either UnpackError BaseFrame
      maybeUnpackedMessage `shouldBe` (Left InvalidVersion)

    it "fails to unpack frames with wrong format" $ do
      let messageWithInvalidFrame = MP.pack ("message"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidFrame :: Either UnpackError BaseFrame
      maybeUnpackedMessage `shouldBe` (Left FrameFormatError)

    it "fails to unpack messages with wrong format" $ do
      let messageWithInvalidMessage = MP.pack (1::Int, "invalidmessage"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidMessage :: Either UnpackError BaseFrame
      maybeUnpackedMessage `shouldBe` (Left MessageFormatError)

    it "fails to unpack messages with an invalid signature" $ do
      let validMessage = MP.pack ("authur"::B.ByteString, []::[B.ByteString], "\0"::B.ByteString, "example"::B.ByteString)
      let messageWithInvalidSignature = MP.pack (1::Int, validMessage, "invalid signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidSignature :: Either UnpackError BaseFrame
      maybeUnpackedMessage `shouldBe` (Left InvalidSignature)

main :: IO ()
main = hspec $ do
  frameSpec
