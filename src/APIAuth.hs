{-# LANGUAGE OverloadedStrings #-}

module APIAuth
( apiAuth
, canonicalForm
, digest
, md5Digest
, getPath
, digestToString
)
where

import           Control.Arrow
import qualified Data.ByteString.Base64     as Base64   (encode)
import qualified Data.ByteString.Char8      as B8       (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL8      (pack, unpack, toStrict, fromStrict)
import           Data.Digest.Pure.SHA                   (hmacSha1
                                                        ,bytestringDigest
                                                        ,Digest
                                                        ,SHA1State)
import           Data.Digest.Pure.MD5                   (md5, md5DigestBytes)
import           Data.List                              (intercalate)
import           Network.HTTP.Types.URI     as URITypes (extractPath)

apiAuth :: String -> String -> String -> String
apiAuth client key message = "APIAuth " ++ client ++ ":" ++ (digestToString $ digest key message)

canonicalForm :: String -> String -> String -> String -> String -> String
canonicalForm httpMethod contentType contentMD5 uriPath timestamp =
  intercalate "," [httpMethod, contentType, contentMD5, uriPath, timestamp]

digest :: String -> String -> Digest SHA1State
digest key message = hmacSha1 key' message'
  where
    key'     = BL8.pack key
    message' = BL8.pack message

md5Digest :: String -> String
md5Digest =
  BL8.pack
  >>> md5
  >>> md5DigestBytes
  >>> Base64.encode
  >>> B8.unpack

getPath :: String -> String
getPath =
  B8.pack
  >>> URITypes.extractPath
  >>> B8.unpack

digestToString :: Digest SHA1State -> String
digestToString =
  bytestringDigest
  >>> BL8.toStrict
  >>> Base64.encode
  >>> BL8.fromStrict
  >>> BL8.unpack
