{-# LANGUAGE OverloadedStrings #-}

module CommandParsing (
  parse,
  parseBool,
  parseList,
  parseArgsUrl,
  parseHeader,
  parseHeaders,
  findHeader
) where

import qualified Data.ByteString.Char8     as B8     (pack, unpack)
import qualified Data.CaseInsensitive      as CI     (mk)
import           Data.List                           (find)
import qualified Data.Text                 as T      (pack, unpack, strip, dropWhile)
import           Network.HTTP.Types.Header as HTypes (Header, HeaderName)

parse :: String -> [String] -> String
parse key args
  | elem key args = head . tail $ dropWhile (/= key) args
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
parseArgsUrl = last

parseHeader :: String -> HTypes.Header
parseHeader x = (hHdrCustom, value)
  where
    name = takeWhile (/= ':') x
    hHdrCustom :: HTypes.HeaderName
    hHdrCustom = CI.mk . B8.pack $ name
    value = B8.pack . T.unpack . T.strip $ T.dropWhile (== ':') (T.dropWhile (/= ':') (T.pack x))

parseHeaders :: [String] -> [HTypes.Header]
parseHeaders x = map parseHeader (parseList "-H" x)

findHeader :: String -> [Header] -> String
findHeader headerName headers = value
  where
    hName :: HTypes.HeaderName
    hName = CI.mk . B8.pack $ headerName
    result = find (\x -> fst x == hName) headers
    value = case result of
      Nothing -> ""
      Just y  -> B8.unpack . snd $ y
