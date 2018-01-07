{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NetworkSpec where

import qualified Data.ByteString.Lazy as L
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class
import Network.Multicast
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Test.Hspec
import Kutyus


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
