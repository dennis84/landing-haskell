{-# LANGUAGE OverloadedStrings #-}

module Landing.Repo
  ( Repo(..)
  , fromText
  , joinPath
  ) where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Landing.Util (textToString)

data Repo = Repo { user :: String
                 , repo :: String
                 , ref  :: (Maybe String)
                 } deriving (Show)

fromText :: Text -> Text -> Maybe Text -> Repo
fromText user repo ref =
  Repo (textToString user) (textToString repo) $ fmap textToString ref

joinPath :: Repo -> String
joinPath (Repo user repo ref) = concat $
  [user, "/", repo, "/", fromMaybe "master" ref]
