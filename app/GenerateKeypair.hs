module Main where

import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import Data.Maybe

import Kutyus

getFilename :: IO (Maybe String)
getFilename = listToMaybe <$> getArgs

writeKeys :: String -> (B.ByteString, B.ByteString) -> IO ()
writeKeys filename (publicKey, privateKey) = do
  B.writeFile (filename ++ ".pub") publicKey
  B.writeFile (filename ++ ".priv") privateKey

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
