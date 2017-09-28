{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( version
    , parse
    , parseBool
    , parseList
    , parseArgsUrl
    , parseHeader
    , parseHeaders
    , fetchHttp
    ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.CaseInsensitive       as CI
import           Data.Digest.Pure.SHA
import           Data.Digest.Pure.MD5
import           Data.List
import           Data.List.Split
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.String.Utils
import qualified Data.Text                  as T
import           Data.Time
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header  as HTypes
import           Network.HTTP.Types.URI     as URITypes

version = "photon 0.1.2\n"
       ++ "Protocols: http https\n"
       ++ "Features: Api-Auth SSL"

parse :: String -> [String] -> String
parse key args
  | elem key args = head $ tail (dropWhile (/= key) args)
  | otherwise     = ""

parseBool :: String -> [String] -> Bool
parseBool key args
  | elem key args = True
  | otherwise     = False

parseList :: String -> [String] -> [String]
parseList key args
  | elem key args = parse key args : parseList key (tail (dropWhile (/= key) args))
  | otherwise     = []

parseArgsUrl :: [String] -> String
parseArgsUrl x = last x

parseHeader :: String -> HTypes.Header
parseHeader x = (hHdrCustom, value)
  where
    name = takeWhile (/= ':') x
    hHdrCustom :: HTypes.HeaderName
    hHdrCustom = CI.mk $ B8.pack name
    value = B8.pack
          $ strip
          $ dropWhile (== ':') (dropWhile (/= ':') x)

parseHeaders :: [String] -> [HTypes.Header]
parseHeaders x = map parseHeader (parseList "-H" x)

formatRFC1123 :: UTCTime -> T.Text
formatRFC1123 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X GMT"

httpTime :: UTCTime -> String
httpTime x = T.unpack $ formatRFC1123 x

getPath :: String -> String
getPath x = B8.unpack $ URITypes.extractPath $ B8.pack x

type EmptyMD5 = String
emptyMD5 = "1B2M2Y8AsgTpgAmY7PhCfg=="

contentMD5 :: String -> String
contentMD5 x
  | x == ""   = emptyMD5
  | otherwise = B8.unpack $ md5DigestBytes $ md5 $ BL8.pack x

findHeader :: String -> [Header] -> String
findHeader headerName headers = value
  where
    hName :: HTypes.HeaderName
    hName = CI.mk $ B8.pack headerName
    result = find (\x -> fst x == hName) headers
    value = case result of
      Nothing -> ""
      Just y  -> B8.unpack $ snd y

canonicalForm :: String -> String -> String -> String -> String -> String
canonicalForm method contentType contentMD5 uriPath timestamp =
  intercalate "," [method, contentType, contentMD5, uriPath, timestamp]

authHeader :: String -> String -> String -> HTypes.Header
authHeader client key message = (HTypes.hAuthorization, value)
  where
    key'     = BL8.pack key
    message' = BL8.pack message
    digest   = BL8.unpack
             $ BL8.fromStrict
             $ Base64.encode
             $ BL8.toStrict
             $ bytestringDigest
             $ hmacSha1 key' message'
    value    = B8.pack $ "APIAuth " ++ client ++ ":" ++ digest

getBody :: String -> IO String
getBody x = body
  where
    body | startswith "@" x = readFile $ dropWhile (== '@') x
         | otherwise        = return x

fetchHttp :: String -> String -> String -> String -> [HTypes.Header] -> String -> Bool -> IO String
fetchHttp method url' client key headers body' pretty = do
  let url            | startswith "http://"  url' = url'
                     | startswith "https://" url' = url'
                     | otherwise                  = "http://" ++ url'

  request'          <- parseRequest $ method ++ " " ++ url
  timestamp         <- getCurrentTime
  body              <- getBody body'

  let canonical      = canonicalForm
                       method
                       (findHeader "Content-Type" headers)
                       (contentMD5 body)
                       (getPath    url)
                       (httpTime   timestamp)

      signedHeaders  = headers ++ [
                       (authHeader client key canonical),
                       (HTypes.hDate,              B8.pack $ httpTime timestamp),
                       (HTypes.hContentMD5,        B8.pack $ contentMD5 body)]

      requestHeaders | client == "" && key == "" = headers
                     | otherwise                 = signedHeaders

      request        = setRequestMethod  (B8.pack method)
                     $ setRequestHeaders requestHeaders
                     $ setRequestBodyLBS (BL8.pack body)
                     $ request'

  response          <- httpLbs request

  let responseBody = getResponseBody response
      ppJson       = encodePretty $ (decode responseBody :: Maybe Value)
      result       | pretty == True = ppJson
                   | otherwise      = responseBody

  return $ BL8.unpack result
