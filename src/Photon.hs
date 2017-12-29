{-# LANGUAGE OverloadedStrings #-}

module Photon (
  version,
  versionInfo,
  parse,
  parseBool,
  parseList,
  parseArgsUrl,
  parseHeader,
  parseHeaders,
  send
) where

import           Data.Version                         (showVersion)
import           Paths_photon                         (version)
import           Data.Aeson                           (decode, Value)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteString.Char8      as B8     (pack)
import qualified Data.ByteString.Lazy.Char8 as BL8    (pack, toStrict)
import qualified Data.ByteString.UTF8       as BU     (toString)
import           Data.List                            (isPrefixOf, isInfixOf)
import           Data.Time                            (getCurrentTime)
import           Network.HTTP.Simple                  (Request,
                                                       parseRequest,
                                                       setRequestMethod,
                                                       setRequestHeaders,
                                                       setRequestBodyLBS,
                                                       httpLBS,
                                                       getResponseBody)
import           Network.HTTP.Types.Header  as HTypes (Header, hDate, hContentMD5)
import           APIAuth
import           CommandParsing
import           TimeUtils

versionInfo :: String
versionInfo = "photon " ++ showVersion version ++ "\n"
           ++ "Protocols: http https\n"
           ++ "Features: Api-Auth SSL"

fixUrl :: String -> String
fixUrl url
  | "http://"  `isPrefixOf` url = url
  | "https://" `isPrefixOf` url = url
  | otherwise                   = "http://" ++ url

getBody :: String -> IO String
getBody x = body
  where
    body | isPrefixOf "@" x = readFile . dropWhile (== '@') $ x
         | otherwise        = return x

makeRequest :: String -> String -> String -> String -> [HTypes.Header] -> String -> Bool -> IO Request
makeRequest httpMethod url client key headers body pretty = do
  let url' = fixUrl url

  currentTime       <- getCurrentTime
  request'          <- parseRequest $ httpMethod ++ " " ++ url'
  body'             <- getBody body

  let
      timestamp      = httpTime currentTime
      contentMD5     = md5Digest body'
      canonical      = canonicalForm httpMethod
                                     (findHeader "Content-Type" headers)
                                     contentMD5
                                     (getPath    url')
                                     timestamp

      signedHeaders  = headers ++ [(authHeader client key canonical)
                                  ,(HTypes.hDate,       B8.pack timestamp)
                                  ,(HTypes.hContentMD5, B8.pack contentMD5)]

      requestHeaders | client == "" && key == "" = headers
                     | otherwise                 = signedHeaders

      request        = setRequestMethod  (B8.pack httpMethod)
                     . setRequestHeaders requestHeaders
                     . setRequestBodyLBS (BL8.pack body')
                     $ request'

  return request

send :: String -> String -> String -> String -> [HTypes.Header] -> String -> Bool -> IO String
send httpMethod url client key headers body pretty = do
  request  <- makeRequest httpMethod url client key headers body pretty
  response <- httpLBS request

  let responseBody = getResponseBody response
      ppJson       = encodePretty (decode responseBody :: Maybe Value)
      result       | pretty == True = ppJson
                   | otherwise      = responseBody

  return . BU.toString . BL8.toStrict $ result
