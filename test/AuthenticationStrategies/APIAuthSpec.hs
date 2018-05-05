{-# LANGUAGE OverloadedStrings #-}

module AuthenticationStrategies.APIAuthSpec where

import Test.Hspec
import AuthenticationStrategies.APIAuth
import Data.Time

spec :: Spec
spec = do

  describe "apiAuth" $ do
    context "given client, key and message" $ do
      it "returns an API Auth header string" $ do
        let client  = "user"
            key     = "secretkey"
            message = "GET,application/json,1B2M2Y8AsgTpgAmY7PhCfg==,/,Thu, 18 Jan 2018 18:25:54 GMT"
        shouldBe
          (apiAuth client key message)
          "APIAuth user:UW/f6nZHZFNXxPKyR4Ci+D8kg54="

  describe "canonicalForm" $ do
    context "given http method, content type, content MD5, uri path, and RFC1123 timestamp" $ do
      it "returns an API Auth canonical string" $ do
        let httpMethod  = "GET"
            contentType = "application/json"
            contentMD5  = "1B2M2Y8AsgTpgAmY7PhCfg=="
            uriPath     = "/"
            timestamp   = "Thu, 18 Jan 2018 18:25:54 GMT"
        shouldBe
          (canonicalForm httpMethod contentType contentMD5 uriPath timestamp)
          "GET,application/json,1B2M2Y8AsgTpgAmY7PhCfg==,/,Thu, 18 Jan 2018 18:25:54 GMT"

  describe "digest" $ do
    context "given a key and message" $ do
      it "Creates an HMAC digest" $ do
        let key = "secret"
            message = "What's up world!"
        shouldBe
          (digestToString (digest key message))
          "FUs3lQEU1wjcok5ka8EP7KOX8Cs="

  describe "md5Digest" $ do
    context "given an empty string" $ do
      it "returns an empty MD5 digest" $ do
        let emptyMD5 = "1B2M2Y8AsgTpgAmY7PhCfg=="
        shouldBe
          (md5Digest "")
          emptyMD5
    context "given a non-empty string" $ do
      it "returns an MD5 digest of the string" $ do
        shouldBe
          (md5Digest "Hello World")
          "sQqNsWTgdUEFt6mb5y4/5Q=="

  describe "getPath" $ do
    context "given a url string" $ do
      it "returns just the path of the url" $ do
        shouldBe
          (getPath "http://localhost/resource/id")
          "/resource/id"
