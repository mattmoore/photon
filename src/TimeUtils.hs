{-# LANGUAGE OverloadedStrings #-}

module TimeUtils (httpTime) where

import qualified Data.Text as T (Text, pack, unpack)
import           Data.Time      (UTCTime, formatTime, defaultTimeLocale)

formatRFC1123 :: UTCTime -> T.Text
formatRFC1123 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X GMT"

httpTime :: UTCTime -> String
httpTime x = T.unpack . formatRFC1123 $ x
