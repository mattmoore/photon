{-# LANGUAGE OverloadedStrings #-}

module Main where

import Photon
import Data.List          (isPrefixOf)
import System.Environment (getArgs)

main = do
  args <- getArgs
  case (validate args) of
    Nothing -> processRequest args
    Just x  -> putStrLn x

validate x
  | length x < 1              = Just "You didn't provide any arguments. Try: photon [URL]"
  | (parseBool "--help"    x) = Just help
  | (parseBool "--version" x) = Just versionInfo
  | (parseBool "-V"        x) = Just versionInfo
  | "-" `isPrefixOf` (last x) = Just ("\"" ++ (last x) ++ "\" is invalid. The last argument must be a URL.")
  | otherwise                 = Nothing

processRequest args = do
  let url     = parseArgsUrl         args
      client  = parse "--client"     args
      key     = parse "--key"        args
      method' = parse "-X"           args
      method  | method' == "" = "GET"
              | otherwise     = method'
      headers = parseHeaders         args
      body    = parse "-d"           args
      pretty  = parseBool "--pretty" args

  response <- send method url client key headers body pretty
  putStrLn response

help = "Usage: photon [args] [URL]\n"
    ++ "--version -V:  Version.\n"
    ++ "--client:      Access ID used for authentication.\n"
    ++ "--key:         Secret key used for authentication.\n"
    ++ " -X:           HTTP verb. Default is GET.\n"
    ++ " -d:           Data to send with the request. To include file contents, pass @ as the first character, followed by a file path.\n"
    ++ " -H:           Specify request headers. For example: -H \"x-custom-header: somevalue\""
