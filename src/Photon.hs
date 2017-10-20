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
  fetchHttp
) where

import           Data.Version (showVersion)
import           Paths_photon (version)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.UTF8       as BU
import           Data.List
import           Data.String.Utils
import           Data.Time
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header  as HTypes
import           APIAuth
import           CommandParsing
import           TimeUtils

versionInfo = "photon " ++ showVersion version ++ "\n"
           ++ "Protocols: http https\n"
           ++ "Features: Api-Auth SSL"

getBody :: String -> IO String
getBody x = body
  where
    body | startswith "@" x = readFile . dropWhile (== '@') $ x
         | otherwise        = return x

fetchHttp :: String -> String -> String -> String -> [HTypes.Header] -> String -> Bool -> IO String
fetchHttp httpMethod url' client key headers body' pretty = do
  let url            | "http://"  `isPrefixOf` url' = url'
                     | "https://" `isPrefixOf` url' = url'
                     | otherwise                    = "http://" ++ url'

  request'          <- parseRequest $ httpMethod ++ " " ++ url
  timestamp         <- getCurrentTime
  body              <- getBody body'

  let canonical      = canonicalForm
                       httpMethod
                       (findHeader "Content-Type" headers)
                       (contentMD5 body)
                       (getPath    url)
                       (httpTime   timestamp)

      signedHeaders  = headers ++ [
                       (authHeader client key canonical),
                       (HTypes.hDate,       B8.pack . httpTime $ timestamp),
                       (HTypes.hContentMD5, B8.pack . contentMD5 $ body)]

      requestHeaders | client == "" && key == "" = headers
                     | otherwise                 = signedHeaders

      request        = setRequestMethod  (B8.pack httpMethod)
                     . setRequestHeaders requestHeaders
                     . setRequestBodyLBS (BL8.pack body)
                     $ request'

  response          <- httpLbs request

  let responseBody = getResponseBody response
      ppJson       = encodePretty (decode responseBody :: Maybe Value)
      result       | pretty == True = ppJson
                   | otherwise      = responseBody

  return . BU.toString . BL8.toStrict $ result
