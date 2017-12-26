module Main where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.Environment (getArgs)
import Data.Maybe

import Kutyus

readKeys :: String -> IO (Maybe (S.ByteString, S.ByteString))
readKeys keyname = do
  pubkey <- S.readFile (keyname ++ ".pub")
  privkey <- S.readFile (keyname ++ ".priv")
  return $ Just (pubkey, privkey)

getKeys :: IO (Maybe (S.ByteString, S.ByteString))
getKeys = do
  maybeKeyname <- listToMaybe <$> getArgs
  case maybeKeyname of
    Nothing -> return Nothing
    Just name -> readKeys name

usage :: IO ()
usage = putStrLn "generate-message <keyname>"

generateMessage :: PublicKey -> PrivateKey -> IO ()
generateMessage pub priv = do
  content <- L.getContents
  let msg = packMessage priv (Message (AuthorId pub) Nothing Blob content)
  L.putStrLn msg

main :: IO ()
main = do
  keys <- getKeys
  case keys of
    Nothing -> usage
    Just (pub, priv) -> generateMessage (PublicKey pub) (PrivateKey priv)

