{-# LANGUAGE OverloadedStrings #-}

import Kutyus
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 (pack)

startReceiver :: IO ()
startReceiver = do
  receiverChannel <- newTChanIO
  forkIO $ startBroadcastReceiver receiverChannel
  forkIO $ startPrinter receiverChannel
  pure ()

startPrinter :: BroadcastChannel -> IO ()
startPrinter chan = forever $ do
  msg <- atomically $ readTChan chan
  L.putStrLn msg

startSender :: IO BroadcastChannel
startSender = do
  senderChannel <- newTChanIO
  forkIO $ startBroadcastSender senderChannel
  pure senderChannel

repl :: (L.ByteString -> IO ()) -> IO ()
repl publisher = forever $ do
  putStrLn ">"
  line <- getLine
  publisher (pack line)

broadcastMessage :: BroadcastChannel -> L.ByteString -> IO ()
broadcastMessage chan msg = atomically $ writeTChan chan msg

main :: IO ()
main = do
  startReceiver
  senderChannel <- startSender
  repl (broadcastMessage senderChannel)

