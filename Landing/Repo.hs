{-# LANGUAGE OverloadedStrings #-}

module Landing.Repo
  ( Repo(..)
  , fromText
  , makePath
  ) where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Landing.Util (textToString)

data Repo = Repo String String (Maybe String) deriving (Show)

fromText :: Text -> Text -> Maybe Text -> Repo
fromText user repo ref =
  Repo (textToString user) (textToString repo) $ fmap textToString ref

makePath :: Repo -> String
makePath (Repo user repo ref) = concat
  [ user, "/", repo, "/", fromMaybe "master" ref ]
