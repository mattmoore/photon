{-# LANGUAGE OverloadedStrings #-}

module Photon (
  version
, versionInfo
, parse
, parseBool
, parseList
, parseArgsUrl
, parseHeader
, parseHeaders
, keyType
, apiAuthHeader
, jwtHeader
, send
) where

import           Data.Version                         (showVersion)
import           Paths_photon                         (version)
import           Data.Aeson                           (decode, Value)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteString.Char8      as B8     (pack)
import qualified Data.ByteString.Lazy.Char8 as BL8    (pack, toStrict)
import qualified Data.ByteString.UTF8       as UTF8   (toString, fromString)
import           Data.List                            (isPrefixOf, isInfixOf)
import           Data.Time                            (getCurrentTime)
import           Data.Maybe
import           Network.HTTP.Simple                  (Request
                                                      ,parseRequest
                                                      ,setRequestMethod
                                                      ,setRequestHeaders
                                                      ,setRequestBodyLBS
                                                      ,httpLBS
                                                      ,getResponseBody)
import           Network.HTTP.Types.Header  as HTypes (Header
                                                      ,hAuthorization
                                                      ,hDate
                                                      ,hContentMD5)
import           AuthenticationStrategies.APIAuth
import           AuthenticationStrategies.JWTAuth
import           CommandParsing
import           TimeUtils

versionInfo :: String
versionInfo = "photon " ++ showVersion version ++ "\n"
           ++ "Protocols: http https\n"
           ++ "Features: API-Auth JWT-RSA SSL"

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

data KeyType = APIAuthKey | JWTKey

keyType :: String -> KeyType
keyType ('@':_) = JWTKey
keyType _ = APIAuthKey

apiAuthHeader :: String -> String -> String -> HTypes.Header
apiAuthHeader client key canonical = (HTypes.hAuthorization, UTF8.fromString $ apiAuth client key canonical)

jwtHeader :: String -> String -> IO HTypes.Header
jwtHeader key claims = do
  keyStore <- readPrivateKeyStore (tail key)
  jwtEncoded <- jwtAuth (privateKey keyStore) (UTF8.fromString claims)
  let bearer = UTF8.fromString ("Bearer " ++ (UTF8.toString jwtEncoded))
  return (HTypes.hAuthorization, bearer)

makeRequest :: String -> String -> String -> String -> String -> [HTypes.Header] -> String -> Bool -> IO Request
makeRequest httpMethod url client key jwtClaims headers body pretty = do
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

  authNHeader   <- case keyType key of
                           JWTKey     -> jwtHeader key jwtClaims
                           APIAuthKey -> return (apiAuthHeader client key canonical)

  let
      signedHeaders  = headers ++ [authNHeader
                                  ,(HTypes.hDate,       B8.pack timestamp)
                                  ,(HTypes.hContentMD5, B8.pack contentMD5)]

      requestHeaders | client == "" && key == "" = headers
                     | otherwise                 = signedHeaders

      request        = setRequestMethod  (B8.pack httpMethod)
                     . setRequestHeaders requestHeaders
                     . setRequestBodyLBS (BL8.pack body')
                     $ request'

  return request

send :: String -> String -> String -> String -> String -> [HTypes.Header] -> String -> Bool -> IO String
send httpMethod url client key jwtClaims headers body pretty = do
  request  <- makeRequest httpMethod url client key jwtClaims headers body pretty
  response <- httpLBS request

  let responseBody = getResponseBody response
      ppJson       = encodePretty (decode responseBody :: Maybe Value)
      result       | pretty == True = ppJson
                   | otherwise      = responseBody

  return . UTF8.toString . BL8.toStrict $ result
