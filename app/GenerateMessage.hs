module Main where

import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import Data.Maybe

import Kutyus

readKeys :: String -> IO (Maybe (B.ByteString, B.ByteString))
readKeys keyname = do
  pubkey <- B.readFile (keyname ++ ".pub")
  privkey <- B.readFile (keyname ++ ".priv")
  return $ Just (pubkey, privkey)

getKeys :: IO (Maybe (B.ByteString, B.ByteString))
getKeys = do
  maybeKeyname <- listToMaybe <$> getArgs
  case maybeKeyname of
    Nothing -> return Nothing
    Just name -> readKeys name

usage :: IO ()
usage = putStrLn "generate-message <keyname>"

generateMessage :: B.ByteString -> B.ByteString -> IO ()
generateMessage pub priv = do
  content <- B.getContents
  let msg = packMessage priv (Message (AuthorId pub) Nothing Blob content)
  B.putStrLn msg

main :: IO ()
main = do
  keys <- getKeys
  case keys of
    Nothing -> usage
    Just (pub, priv) -> generateMessage pub priv
