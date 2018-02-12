{-# LANGUAGE OverloadedStrings #-}
module Main where

import Kutyus
import Control.Applicative
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.ByteString.Char8 as Char8
import System.Environment (getArgs, lookupEnv)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

instance FromRow L.ByteString where
  fromRow = field

homePrefix :: String -> IO String
homePrefix str = do
  maybeHome <- lookupEnv("HOME")
  case maybeHome of
    Nothing -> pure $ ".kutyus/" ++ str
    Just h -> pure $ h ++ "/.kutyus/" ++ str

dbPath :: Base64 -> IO String
dbPath (Base64 raw) = homePrefix $ Char8.unpack raw

getKeys :: IO (Maybe (PublicKey, PrivateKey))
getKeys = do
  maybeKeyname <- listToMaybe <$> getArgs
  case maybeKeyname of
    Nothing -> return Nothing
    Just name -> readKeys name

readKeys :: String -> IO (Maybe (PublicKey, PrivateKey))
readKeys keyname = do
  pubkey <- (homePrefix $ keyname ++ ".pub") >>= S.readFile
  privkey <- (homePrefix $ keyname ++ ".priv") >>= S.readFile
  return $ Just (PublicKey pubkey, PrivateKey privkey)

usage :: IO ()
usage = putStrLn "kutyus-db <keyname>"

run :: PublicKey -> PrivateKey -> IO ()
run pub priv = do
  let author = AuthorId pub
  let testMessage = Message author Nothing Blob "third message"
  let (msgId, messageBlob) = packMessage priv $ testMessage
  path <- dbPath (authorIdToBase64 author)
  conn <- open path
  execute_ conn "CREATE TABLE IF NOT EXISTS messages (id TEXT PRIMARY KEY, author TEXT, content_type TEXT, message BLOB)"
  execute conn "INSERT INTO messages (id, author, content_type, message) VALUES (?, ?, ?, ?)" (rawBase64 $ messageIdToBase64 msgId, rawBase64 $ authorIdToBase64 author, "\0" :: S.ByteString, messageBlob)
  r <- query_ conn "SELECT message from messages" :: IO [L.ByteString]
  mapM_ (print . unpackMessage) r
  close conn

main :: IO ()
main = do
  maybeKeys <- getKeys
  case maybeKeys of
    Just (pub, priv) -> run pub priv
    Nothing -> usage
