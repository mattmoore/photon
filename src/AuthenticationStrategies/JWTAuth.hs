{-# LANGUAGE OverloadedStrings #-}

module AuthenticationStrategies.JWTAuth (
  readPrivateKeyStore
, privateKey
, readCertificateStore
, publicKey
, jwtAuth
)
where

import Control.Arrow
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Crypto.PubKey.RSA as RSA
import Crypto.Random.Types
import qualified Data.X509 as X509
import Data.X509.File
import qualified Data.X509.CertificateStore as X509CertStore
import Jose.Jwa
import Jose.Jwk
import Jose.Jwt

jwtAuth :: MonadRandom m => PrivateKey -> B.ByteString -> m (B.ByteString)
jwtAuth key claims = do
  let jwk = RsaPrivateJwk key (Just (KeyId "privateKey")) Nothing Nothing
  Right (Jwt jwt) <- encode [jwk] (JwsEncoding RS256) (Claims claims)
  return jwt

readPrivateKeyStore :: FilePath -> IO [X509.PrivKey]
readPrivateKeyStore file = do
  keyStore <- Data.X509.File.readKeyFile file
  return keyStore

privateKey :: [X509.PrivKey] -> PrivateKey
privateKey keys = key
  where
    X509.PrivKeyRSA key = (head keys)

readCertificateStore :: FilePath -> IO (Maybe X509CertStore.CertificateStore)
readCertificateStore file = do
  maybeCertStore <- X509CertStore.readCertificateStore file
  return $ case maybeCertStore of
    Just cert -> Just cert
    Nothing   -> Nothing

publicKey :: X509CertStore.CertificateStore -> X509.PubKey
publicKey =
  X509CertStore.listCertificates
  >>> head
  >>> X509.getCertificate
  >>> X509.certPubKey
