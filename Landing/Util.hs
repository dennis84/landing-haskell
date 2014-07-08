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
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

textToString :: Text -> String
textToString = T.unpack . T.fromStrict

textToByteString :: Text -> L.ByteString
textToByteString = TE.encodeUtf8 . T.fromStrict

replacePlaceholders
  :: [(S.ByteString, L.ByteString)]
  -> L.ByteString -> L.ByteString
replacePlaceholders xs x =
  foldl (\a (k, v) -> replace k v a) x xs
