{-# LANGUAGE OverloadedStrings #-}

module Kutyus.Network
  ( startBroadcastSender
  , startBroadcastReceiver
  , BroadcastChannel
  , KutyusNetwork(..)
  , Sender(..)
  , Receiver(..)
  ) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy as L
import Network.Multicast
import Network.Socket (Socket, SockAddr)
import Network.Socket.ByteString (sendTo, recv)
import Control.Monad (forever)

type BroadcastChannel = TChan L.ByteString

newtype Sender = Sender (Socket, SockAddr) deriving (Eq, Show)
newtype Receiver = Receiver Socket deriving (Eq, Show)

class (Monad m) => KutyusNetwork m where
  createSender :: m Sender
  broadcast :: Sender -> L.ByteString -> m ()
  createReceiver :: m Receiver
  receive :: Receiver -> m L.ByteString
  readChannel :: BroadcastChannel -> m L.ByteString
  writeChannel :: BroadcastChannel -> L.ByteString -> m ()

kutyusAddress :: String
kutyusAddress = "224.0.0.99"

kutyusPort :: Num a => a
kutyusPort = 9999

maxMessageSize :: Int
maxMessageSize = 1024 * 1024

instance KutyusNetwork IO where
  createSender = Sender <$> multicastSender kutyusAddress kutyusPort
  broadcast (Sender (socket, addr)) message = sendTo socket (L.toStrict message) addr >> pure ()
  createReceiver = Receiver <$> multicastReceiver kutyusAddress kutyusPort
  receive (Receiver socket) = L.fromStrict <$> recv socket maxMessageSize
  readChannel = atomically . readTChan
  writeChannel chan message = atomically $ writeTChan chan message

startBroadcastSender :: KutyusNetwork m => BroadcastChannel -> m ()
startBroadcastSender chan = do
  sender <- createSender
  pure ()
  senderLoop chan sender

senderLoop :: KutyusNetwork m => BroadcastChannel -> Sender -> m ()
senderLoop chan sender = forever $ do
  message <- readChannel chan
  broadcast sender message

startBroadcastReceiver :: KutyusNetwork m => BroadcastChannel -> m ()
startBroadcastReceiver chan = do
  receiver <- createReceiver
  receiverLoop chan receiver

receiverLoop :: KutyusNetwork m => BroadcastChannel -> Receiver -> m ()
receiverLoop chan receiver = forever $ do
  message <- receive receiver
  writeChannel chan message
