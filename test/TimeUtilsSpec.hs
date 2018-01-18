{-# LANGUAGE OverloadedStrings #-}

module TimeUtilsSpec where

import Test.Hspec
import TimeUtils
import Data.Time

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "httpTime" $ do
    context "given a UTCTime" $ do
      it "returns an RFC1123 formatted Text representation" $ do
        let day     = fromGregorian 2018 1 1
            seconds = 12*60^2 + 35*60 + 56
            time    = UTCTime day seconds
        shouldBe
          (httpTime time)
          "Mon, 01 Jan 2018 12:35:56 GMT"
