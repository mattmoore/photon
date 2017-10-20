{-# LANGUAGE OverloadedStrings #-}

module TimeUtils (httpTime) where

import qualified Data.Text as T
import           Data.Time

formatRFC1123 :: UTCTime -> T.Text
formatRFC1123 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X GMT"

httpTime :: UTCTime -> String
httpTime x = T.unpack . formatRFC1123 $ x
