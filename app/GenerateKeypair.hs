module Main where

import qualified Data.ByteString as B
import System.Environment (getArgs)
import Data.Maybe

import Kutyus

getFilename :: IO (Maybe String)
getFilename = listToMaybe <$> getArgs

writeKeys :: String -> (PublicKey, PrivateKey) -> IO ()
writeKeys filename (publicKey, privateKey) = do
  B.writeFile (filename ++ ".pub") (rawPublicKey publicKey)
  B.writeFile (filename ++ ".priv") (rawPrivateKey privateKey)

usage :: IO ()
usage = putStrLn "generate-kutyus-keypair <keyname>"

main :: IO ()
main = do
  maybeFilename <- getFilename
  case maybeFilename of
    Just fn -> do
      keys <- generateKeypair
      writeKeys fn keys
    Nothing -> usage
