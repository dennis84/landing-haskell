{-# LANGUAGE OverloadedStrings #-}

module Landing.Util
  ( textToString
  , textToByteString
  , replacePlaceholders
  ) where

import Data.Text (Text)
import Data.ByteString.Lazy.Search (replace)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE

textToString = T.unpack . T.fromStrict

textToByteString = TE.encodeUtf8 . T.fromStrict

replacePlaceholders xs x =
  foldl (\a (k, v) -> replace k v a) x xs
