{-# LANGUAGE OverloadedStrings #-}

module Landing.Markdown (parseMarkdown) where

import Text.Pandoc
import qualified Data.ByteString.Lazy.Char8 as C

parseMarkdown :: C.ByteString -> C.ByteString
parseMarkdown = C.pack . markdownToHtmlString . C.unpack

markdownToHtmlString :: String -> String
markdownToHtmlString =
  (writeHtmlString def
    { writerReferenceLinks = True
    , writerSectionDivs = True
    , writerExtensions = githubMarkdownExtensions
    , writerHighlight = True })
  . readMarkdown def
    { readerExtensions = githubMarkdownExtensions }
