module Main where

import qualified Data.ByteString.Lazy as B

import Kutyus

showMessage :: Message B.ByteString -> IO ()
showMessage (Message author parent contentType content) = do
  print $ author
  print $ parent
  print $ contentType
  print $ content

main :: IO ()
main = do
  content <- B.getContents
  case unpackMessage content of
    Right message -> showMessage message
    Left err -> putStrLn $ "Failed to unpack message: " ++ (show err)
