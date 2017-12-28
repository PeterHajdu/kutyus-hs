{-# LANGUAGE OverloadedStrings #-}

import Kutyus
import Network.Socket (withSocketsDo, sendTo, recvFrom)
import Network.Multicast
import Control.Monad (forever)
import Control.Concurrent (forkIO)

publish :: String -> IO ()
publish message = do
  withSocketsDo $ do
    (sock, addr) <- multicastSender "224.0.0.99" 9999
    sendTo sock message addr
    putStrLn $ "message sent: " ++ message
  pure ()

repl :: (String -> IO ()) -> IO ()
repl handler = forever $ do
  putStrLn "> "
  line <- getLine
  handler line

receiver :: IO ()
receiver = do
  sock <- multicastReceiver "224.0.0.99" 9999
  forever $ do
    (msg, _, addr) <- recvFrom sock 1024
    putStrLn $ "Recv [" ++ show addr ++ "]: " ++ msg

main :: IO ()
main = do
  forkIO receiver
  repl publish
