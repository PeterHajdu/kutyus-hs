{-# LANGUAGE OverloadedStrings #-}

import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B

import Test.Hspec

import Kutyus

frameSpec :: Spec
frameSpec =
  describe "frames" $ do
    it "fails to unpack messages with versions oher than 1" $ do
      let messageWithInvalidVersion = MP.pack (0::Int, "message"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidVersion :: Either UnpackError BaseFrame
      maybeUnpackedMessage `shouldBe` (Left InvalidVersion)

    it "fails to unpack messages with wrong format" $ do
      let messageWithInvalidVersion = MP.pack ("message"::B.ByteString, "signature"::B.ByteString) :: B.ByteString
      let maybeUnpackedMessage = unpackFrame messageWithInvalidVersion :: Either UnpackError BaseFrame
      maybeUnpackedMessage `shouldBe` (Left FrameFormatError)

main :: IO ()
main = hspec $ do
  frameSpec
