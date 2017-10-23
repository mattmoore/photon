{-# LANGUAGE OverloadedStrings #-}

module APIAuth where

import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Digest.Pure.SHA
import           Data.Digest.Pure.MD5
import           Data.List
import           Network.HTTP.Types.Header  as HTypes
import           Network.HTTP.Types.URI     as URITypes

getPath :: String -> String
getPath x = B8.unpack . URITypes.extractPath $ B8.pack x

emptyMD5 :: String
emptyMD5 = "1B2M2Y8AsgTpgAmY7PhCfg=="

md5Digest :: String -> String
md5Digest x
  | x == ""   = emptyMD5
  | otherwise = B8.unpack
              . Base64.encode
              . md5DigestBytes
              . md5
              $ BL8.pack x

canonicalForm :: String -> String -> String -> String -> String -> String
canonicalForm httpMethod contentType contentMD5 uriPath timestamp =
  intercalate "," [httpMethod, contentType, contentMD5, uriPath, timestamp]

authHeader :: String -> String -> String -> HTypes.Header
authHeader client key message = (HTypes.hAuthorization, value)
  where
    key'     = BL8.pack key
    message' = BL8.pack message
    digest   = BL8.unpack
             . BL8.fromStrict
             . Base64.encode
             . BL8.toStrict
             . bytestringDigest
             $ hmacSha1 key' message'
    value    = B8.pack $ "APIAuth " ++ client ++ ":" ++ digest
