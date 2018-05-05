{-# LANGUAGE OverloadedStrings #-}

module AuthenticationStrategies.JWTAuthSpec where

import Test.Hspec
import AuthenticationStrategies.JWTAuth
import Data.Time
import Data.X509
import Data.X509.File
import qualified Data.X509.CertificateStore as X509CertStore

spec :: Spec
spec = do

  describe "readPrivateKeyStore" $ do
    context "given a private key store file" $ do
      it "returns the first key" $ do
        let file = "test/keys/private.pem"
        expectedKeyStore <- readKeyFile file
        let PrivKeyRSA key = head expectedKeyStore
        keyStore <- readPrivateKeyStore file
        shouldBe
          (privateKey keyStore)
          key

  describe "readCertificateStore" $ do
    context "given a public certificate store file" $ do
      it "returns the first certificate's public key" $ do
        let file = "test/keys/certificate.pem"
        expectedCertStore <- X509CertStore.readCertificateStore file
        let Just expectedCertificate = expectedCertStore
        let expectedPublicKey = certPubKey
                              . getCertificate
                              . head
                              . X509CertStore.listCertificates
                              $ expectedCertificate
        Just cert <- readCertificateStore file
        shouldBe
          (publicKey cert)
          expectedPublicKey

  describe "jwtAuth" $ do
    context "given a private key" $ do
      it "creates a signed JWT" $ do
        let file = "test/keys/private.pem"
        privateKeyStore <- readPrivateKeyStore file
        let key = privateKey privateKeyStore
        jws <- jwtAuth key "test message"
        shouldBe
          jws
          "eyJhbGciOiJSUzI1NiIsImtpZCI6InByaXZhdGVLZXkifQ.dGVzdCBtZXNzYWdl.zc1pdK6oR9p0w8IpHpUoHfwCoG9rEcvNlEvZYhF4pBlT8m-6jWj4Nye7SjUjCu_h5KYu2VahS2ZekyFfsc6XsEBLkfaILsv9yC6bFOB95v1DQQ9r941MVCKcNSdhKswXQxYjBJyWL8FimudNTKUClqUBdY6dKKEbNtlnuyrXU3J188AbvFHvxfPE5Ca_rI7zKLiNZoH70GMH_ntFvUw0zIqAGl99qhg34nZ8LuR6qzSf2QjtUYKwOasBNP209nFDIGKJudqkXYV6llWAvYdFuC8aJOeUscI8nHuDgGq6oVlKIUt2XsBvpaGUTEhm8pXbl4VSR-1zJwl1_G-IRoxpVH0gVCtlPZBXWwxYhXlETLtEQ_rqMo059bFH8b4BApqy_9hJP_W0trrlwebOuHLIQ82t-wqV94aYKXjfi142GKRlyvCJZHq6-Ba_jNEAl59dV0tdmNYS_uTxxZ4lWVhmGwsPfRsjfXJYZlPGcOqTTbpNc2agcVQtvoo2VAyPg1diOE-pHI9NPm9SSzK2MZ9jocOLyZvoZq-HDHNMgLxJyaZkRXY1FftqyC0TSgMCMqmcWQRNudiosKmli-u9xJvI7hl1HLtYOVrKhPSRNjFDz4KV1RNP1pfl7T_oEEo_2k62WgqFhPeTjPtVfnmBlz13EOPo0JiB8ZNPvOI6wp78SiE"
