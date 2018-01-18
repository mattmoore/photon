{-# LANGUAGE OverloadedStrings #-}

module CommandParsingSpec where

import Test.Hspec
import CommandParsing
import Network.HTTP.Types.Header

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "parse" $ do
    context "given a lookup key and list of arguments" $ do
      it "returns the value for the given key" $ do
        let args = [ "--client", "user",
                     "--key", "password",
                     "-H", "content-type: application/json",
                     "-d", "some data" ]
        shouldBe
          (parse "-H" args)
          "content-type: application/json"
        shouldBe
          (parse "--client" args)
          "user"
        shouldBe
          (parse "--key" args)
          "password"
        shouldBe
          (parse "-d" args)
          "some data"

  describe "parseBool" $ do
    context "given a flag that exists" $ do
      it "returns True" $ do
        let args = ["--pretty"]
        shouldBe
          (parseBool "--pretty" args)
          True
    context "given a flag that does not exist" $ do
      it "returns False" $ do
        shouldBe
          (parseBool "--pretty" [])
          False

  describe "parseList" $ do
    context "given a list of repeating arguments" $ do
      it "returns a list of found arguments" $ do
        let args = [ "-H", "content-type: application/json",
                     "-H", "x-custom-header: 1" ]
        shouldBe
          (parseList "-H" args)
          ["content-type: application/json", "x-custom-header: 1"]

  describe "parseArgsUrl" $ do
    context "given a list of args including url" $ do
      it "returns the url value" $ do
        let args = [ "-H", "content-type: application/json"
                   , "http://localhost/resource/id" ]
        shouldBe
          (parseArgsUrl args)
          "http://localhost/resource/id"

  describe "parseHeader" $ do
    context "given a string header" $ do
      it "parses the header and returns a typed header" $ do
        let header = "content-type: application/json"
        shouldBe
          (parseHeader header)
          (hContentType, "application/json")

  describe "parseHeaders" $ do
    context "given a list of string headers" $ do
      it "parses the header and returns a typed header" $ do
        let headers = [ "-H", "content-type: application/json"
                      , "-H", "authorization: letmein!" ]
        shouldBe
          (parseHeaders headers)
          [(hContentType, "application/json"), (hAuthorization, "letmein!")]

  describe "findHeader" $ do
    context "given a header name to search for and a list of headers containing the header" $ do
      it "finds the header if it exists" $ do
        let headers = parseHeaders ["-H", "content-type: application/json"]
        shouldBe
          (findHeader "content-type" headers)
          "application/json"
    context "given a header name to search for and a list of headers missing the header" $ do
      it "finds the header if it exists" $ do
        let headers = parseHeaders ["-H", "authorization: allowme!"]
        shouldBe
          (findHeader "content-type" headers)
          ""
