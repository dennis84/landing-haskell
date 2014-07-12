{-# LANGUAGE OverloadedStrings #-}

module Landing.Cache
  ( makeCache
  , makeCacheKey
  , lookup
  , insert
  , delete
  , Cache
  ) where

import Prelude hiding (lookup)
import Landing.Util (textToString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B
import qualified Data.Cache.LRU.IO as LRU

type Cache = LRU.AtomicLRU String (B.ByteString, UTCTime)

makeCacheKey :: Text -> Text -> Maybe Text -> String
makeCacheKey user repo ref = concat
  [ textToString user, "/", textToString repo, "/"
  , fromMaybe "master" $ fmap textToString ref ]

makeCache :: IO Cache
makeCache = LRU.newAtomicLRU (Just 256)

lookup :: String -> Cache -> IO (Maybe (B.ByteString, UTCTime))
lookup k c = LRU.lookup k c

insert :: String -> (B.ByteString, UTCTime) -> Cache -> IO ()
insert k v c = LRU.insert k v c

delete :: String -> Cache -> IO ()
delete k c = (LRU.delete k c) >> return ()
