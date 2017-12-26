module Kutyus.Crypto where

import qualified Crypto.Sign.Ed25519 as ED
import Crypto.Hash.SHA512
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

newtype PrivateKey = PrivateKey {rawPrivateKey :: S.ByteString} deriving (Eq, Show)
newtype PublicKey = PublicKey {rawPublicKey :: S.ByteString} deriving (Eq, Show)
newtype Signature = Signature {rawSignature :: S.ByteString} deriving (Eq, Show)
newtype Digest = Digest {rawDigest :: S.ByteString} deriving (Eq, Show)

pubKeyFromLazy :: L.ByteString -> PublicKey
pubKeyFromLazy buffer = PublicKey $ L.toStrict buffer

sigFromLazy :: L.ByteString -> Signature
sigFromLazy buffer = Signature $ L.toStrict buffer

digest :: L.ByteString -> Digest
digest = Digest . hashlazy

verifySignature :: PublicKey -> Signature -> Digest -> Bool
verifySignature (PublicKey pubKey) (Signature sig) (Digest digest) = ED.dverify (ED.PublicKey pubKey) digest (ED.Signature sig)

sign :: PrivateKey -> Digest -> Signature
sign privKey digest = let key = (ED.SecretKey $ rawPrivateKey privKey)
                          sig = ED.dsign key $ rawDigest digest
                       in Signature $ ED.unSignature sig

generateKeypair :: IO (PublicKey, PrivateKey)
generateKeypair = do
  (pub, priv) <- ED.createKeypair
  return ((PublicKey $ ED.unPublicKey pub), (PrivateKey $ ED.unSecretKey priv))

