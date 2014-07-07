{-# LANGUAGE OverloadedStrings #-}

module Landing.Cache
  ( makeCache
  , makeCacheKey
  , cacheLookup
  , cacheInsert
  , cacheDelete
  , LRU
  ) where

import Landing.Util (textToString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString.Lazy as B
import qualified Data.Cache.LRU.IO as Lru

type LRU = Lru.AtomicLRU String (B.ByteString, UTCTime)

makeCacheKey :: Text -> Text -> String
makeCacheKey u r = concat [textToString u, "/", textToString r]

makeCache :: IO LRU
makeCache = Lru.newAtomicLRU (Just 256)

cacheLookup :: String -> LRU -> IO (Maybe (B.ByteString, UTCTime))
cacheLookup k c = Lru.lookup k c

cacheInsert :: String -> (B.ByteString, UTCTime) -> LRU -> IO ()
cacheInsert k v c = Lru.insert k v c

cacheDelete :: String -> LRU -> IO ()
cacheDelete k c = (Lru.delete k c) >> return ()
