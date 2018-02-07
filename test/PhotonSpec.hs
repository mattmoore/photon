{-# LANGUAGE OverloadedStrings #-}

module PhotonSpec where

import Test.Hspec
import Photon
import AuthenticationStrategies.APIAuth
import Data.Time

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "apiAuthHeader" $ do
    context "given a client, key, and canonical string" $ do
      it "generates a valid API Authorization header" $ do
        let client      = "client1"
            key         = "secretkey"
            httpMethod  = "GET"
            contentType = "application/json"
            contentMD5  = "1B2M2Y8AsgTpgAmY7PhCfg=="
            uriPath     = "/"
            timestamp   = "Thu, 18 Jan 2018 18:25:54 GMT"
            canonical   = (canonicalForm httpMethod contentType contentMD5 uriPath timestamp)
        shouldBe
          (show $ apiAuthHeader client key canonical)
          "(\"Authorization\",\"APIAuth client1:UW/f6nZHZFNXxPKyR4Ci+D8kg54=\")"

  describe "jwtHeader" $ do
    context "given a client, key, and claims" $ do
      it "generates a valid JWT Authorization header" $ do
        let client = "client1"
            key    = "@test/keys/private.pem"
            claims = "{\"sub\": \"customer:1\"}"
        authHeader <- jwtHeader key claims
        shouldBe
          (show authHeader)
          "(\"Authorization\",\"Bearer eyJhbGciOiJSUzI1NiIsImtpZCI6InByaXZhdGVLZXkifQ.eyJzdWIiOiAiY3VzdG9tZXI6MSJ9.VIFBzyZpGhjG7PSsyYBI-ndJdKa6Vpzb8NSb30QWYEzJPwT1dIH6EwOnFmuVo5RqXWq0uar68HWcq_K8L9gp1F4AUcPWLzhFoMH_AZrR8ncb06-us7uFw_hKULloFiWOvr2C9lhcvkKgUIyz9gsgbwZ1rZdhLHDHEAL6CpRV9COXrCTFfwshwYzrjLmjAJ3mQWNUpCV-m1gWP2A1ptsiu2VW5aI_pzQ26s0AhE-5h_SfDh875ifWhrlAxeC4MGi8sMDkMM-VkD_Qlu7YnZS66K6reDK4nAVm3TaOXM7RLRSa2fK1aLktRN2N-iCjHRo2A7OX2ER8qsqeyqzie6rk0WpfdC1_r-oCo1HJXvmHNXM4wUJIQs-UC614i-0yr3SikE0j56MwsbRnYV6YTZlTiLq6YM-EAG-fLW2oQYos_RTIA4wVj6idf8q6wfGDaGOW_rFurDzd3d_dlYshyBd9j7I8jPSBAyVvXVfw_QgYsTloiRI3Qe_sV4uQXHbEakjge40f_9ue60WT6obr_t1eYZjPxH4WP3fz_8-K_vUl3h-x-V-bvznnHkRgyt-BDlxbWMGHY-O8kOUc3wGnb9QBDQYiKcvRDsibH57TOMMuo5pUSRidU360Z99aIGRboMP5gy6ItXnEoonLUs2vw_phP-jaN1WGqnB0b00Qr8O1D2s\")"
