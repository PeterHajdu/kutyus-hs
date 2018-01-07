{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Network.Multicast
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.MessagePack as MP
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Sign.Ed25519 as ED
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class
import Data.Either

import Test.Hspec
import Test.QuickCheck

import Kutyus

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

data DummyNetworkEnv = DummyNetworkEnv
  { sender :: Sender
  , networkChan :: BroadcastChannel
  }

dummySender :: IO Sender
dummySender = Sender <$> multicastSender "224.0.0.99" 9999

createDefaultEnv :: IO DummyNetworkEnv
createDefaultEnv = do
  sender <- dummySender
  networkChan <- newTChanIO
  return $ DummyNetworkEnv sender networkChan

newtype DummyNetwork a = DummyNetwork {run :: ReaderT DummyNetworkEnv IO a} deriving (Functor, Applicative, Monad, MonadReader DummyNetworkEnv, MonadIO)

runDummyNetwork :: DummyNetworkEnv -> DummyNetwork a -> IO a
runDummyNetwork env (DummyNetwork st) = runReaderT st env

defaultMessage :: L.ByteString
defaultMessage = "received message"

instance KutyusNetwork DummyNetwork where
  createSender = do
    asks sender

  broadcast _ message = do
    network <- asks networkChan
    liftIO $ atomically $ writeTChan network message

  readChannel = liftIO . atomically . readTChan

  writeChannel chan msg = liftIO $ atomically $ writeTChan chan msg

  createReceiver = do
    (Sender (socket, _)) <- (asks sender)
    pure $ Receiver socket

  receive _ = pure defaultMessage

networkSpec :: Spec
networkSpec =
  describe "network" $ do
    describe "start broadcast sender" $ do
      it "broadcasts messages from the channel" $ do
        let expectedMessage1 = "a message"
        let expectedMessage2 = "another message"
        (broadcastChan, networkChan) <- initNetworkSender
        atomically $ do
          writeTChan broadcastChan expectedMessage1
          writeTChan broadcastChan expectedMessage2
        (m1, m2) <- atomically $ do
          m1 <- readTChan networkChan
          m2 <- readTChan networkChan
          pure (m1, m2)
        (m1, m2) `shouldBe` (expectedMessage1, expectedMessage2)

    describe "start receiver" $ do
      it "receives messages from the network" $ do
        receiverChan <- initNetworkReceiver
        (m1, m2) <- atomically $ do
          m1 <- readTChan receiverChan
          m2 <- readTChan receiverChan
          pure (m1, m2)
        (m1, m2) `shouldBe` (defaultMessage, defaultMessage)

initNetworkSender :: IO (BroadcastChannel, BroadcastChannel)
initNetworkSender = do
  broadcastChan <- newTChanIO
  defaultEnv <- createDefaultEnv
  forkIO $ runDummyNetwork defaultEnv (startBroadcastSender broadcastChan)
  pure (broadcastChan, networkChan defaultEnv)

initNetworkReceiver :: IO BroadcastChannel
initNetworkReceiver = do
  receiverChan <- newTChanIO
  defaultEnv <- createDefaultEnv
  forkIO $ runDummyNetwork defaultEnv (startBroadcastReceiver receiverChan)
  pure receiverChan

main :: IO ()
main =
  hspec $ do
    messageSpec
    idSpec
    networkSpec
