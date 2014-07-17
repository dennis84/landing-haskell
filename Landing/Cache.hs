{-# LANGUAGE OverloadedStrings #-}

module Landing.Cache
  ( makeCache
  , lookup
  , insert
  , delete
  , Cache
  ) where

import Prelude hiding (lookup)
import Landing.Util (textToString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString.Lazy as B
import qualified Data.Cache.LRU.IO as LRU

type Cache = LRU.AtomicLRU String (B.ByteString, UTCTime)

makeCache :: IO Cache
makeCache = LRU.newAtomicLRU (Just 256)

lookup :: String -> Cache -> IO (Maybe (B.ByteString, UTCTime))
lookup = LRU.lookup

insert :: String -> (B.ByteString, UTCTime) -> Cache -> IO ()
insert = LRU.insert

delete :: String -> Cache -> IO ()
delete k c = (LRU.delete k c) >> return ()
